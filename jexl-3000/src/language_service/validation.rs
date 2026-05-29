use super::ast_walk::{
    TransformCall, collect_all_paths, collect_lambda_paths, collect_transforms, resolve_dot_chain,
    resolve_schema_path, resolve_transform_item_path, visit_children_pub,
};
use super::registry::{
    BUILTIN_VARIABLE_NAMES, OutputTypeKind, TRANSFORMER_REGISTRY, TransformerDescriptor,
};
use super::schema::{
    build_schema_index, resolve_array_item_properties, resolve_path, types_for_path,
};
use super::types::{CodeAction, Diagnostic, DiagnosticSeverity, PrimitiveType, SchemaIndex};
use jexl_parser::Parser;
use jexl_parser::ast::{Expr, Expression, OpCode};
use serde_json::Value;

/// Validate an expression and return diagnostics.
///
/// Checks:
/// 1. Parse errors
/// 2. Undefined schema properties
/// 3. Transform input type mismatches
/// 4. Binary operation type incompatibilities
/// 5. Nullable/optional property warnings
#[must_use]
pub fn validate(expr: &str, schema: Option<&Value>, context: Option<&Value>) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // 1. Parse check
    let ast = match Parser::parse(expr) {
        Ok(ast) => ast,
        Err(err) => {
            let (start, end) = parse_error_location(&err);
            diagnostics.push(Diagnostic {
                message: format!("Parse error: {err}"),
                severity: DiagnosticSeverity::Error,
                start,
                end,
                code_actions: vec![],
            });
            return diagnostics;
        }
    };

    validate_ast(&ast, schema, context)
}

/// Validate using a pre-parsed AST, skipping the parse step.
pub fn validate_ast(
    ast: &Expression,
    schema: Option<&Value>,
    context: Option<&Value>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Build schema index if schema provided
    let schema_index = schema.map(build_schema_index);

    if let Some(index) = &schema_index {
        // 2. Undefined property check
        check_undefined_properties(ast, index, context, &mut diagnostics);

        // 3. Transform type mismatch check
        check_transform_types(ast, index, &mut diagnostics);

        // 4. Binary operation type check
        check_binary_ops(ast, index, &mut diagnostics);

        // 5. Nullable/optional warnings
        check_nullable_warnings(ast, index, &mut diagnostics);

        // 6. Lambda body `this.*` path validation
        check_lambda_this_paths(ast, index, &mut diagnostics);
    }

    diagnostics
}

/// Extract a byte-offset range from a parse error.
const fn parse_error_location<T: std::fmt::Debug, E: std::fmt::Debug>(
    err: &jexl_parser::ParseError<usize, T, E>,
) -> (usize, usize) {
    match err {
        jexl_parser::ParseError::InvalidToken { location } => (*location, *location + 1),
        jexl_parser::ParseError::UnrecognizedEof { location, .. } => (*location, *location),
        jexl_parser::ParseError::UnrecognizedToken {
            token: (start, _, end),
            ..
        }
        | jexl_parser::ParseError::ExtraToken {
            token: (start, _, end),
        } => (*start, *end),
        jexl_parser::ParseError::User { .. } => (0, 0),
    }
}

/// Check for property paths that don't exist in the schema.
fn check_undefined_properties(
    ast: &Expression,
    index: &SchemaIndex,
    context: Option<&Value>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let paths = collect_all_paths(ast);
    let registry = &*TRANSFORMER_REGISTRY;

    for prop_path in &paths {
        let root = prop_path.path.split('.').next().unwrap_or(&prop_path.path);

        // Skip built-in variables and transform names
        if BUILTIN_VARIABLE_NAMES.contains(&root) || registry.contains_key(root) {
            continue;
        }

        // Check if root exists in schema
        if resolve_path(root, index).is_none() {
            // Fallback: check context
            if let Some(ctx) = context
                && ctx.get(root).is_some()
            {
                continue;
            }
            let root_candidates: Vec<&str> = index.keys()
                .filter(|k| !k.contains('.'))
                .map(String::as_str)
                .collect();
            let code_actions = find_closest_match(root, &root_candidates, 2)
                .map(|suggestion| vec![CodeAction {
                    title: format!("Did you mean `{suggestion}`?"),
                    replacement: suggestion.to_string(),
                    start: prop_path.location.0,
                    end: prop_path.location.1,
                }])
                .unwrap_or_default();
            diagnostics.push(Diagnostic {
                message: format!("Property `{root}` is not defined in the schema"),
                severity: DiagnosticSeverity::Error,
                start: prop_path.location.0,
                end: prop_path.location.1,
                code_actions,
            });
            continue;
        }

        // Check full path (for multi-segment paths)
        if prop_path.path.contains('.') && resolve_path(&prop_path.path, index).is_none() {
            // Try to find a suggestion for the last segment
            let segments: Vec<&str> = prop_path.path.split('.').collect();
            let last_segment = segments.last().unwrap_or(&"");
            let parent_path = segments[..segments.len() - 1].join(".");
            let code_actions = if let Some(parent_entry) = resolve_path(&parent_path, index) {
                let sibling_candidates: Vec<&str> = parent_entry.properties.iter().map(String::as_str).collect();
                find_closest_match(last_segment, &sibling_candidates, 2)
                    .map(|suggestion| {
                        let mut new_segments = segments[..segments.len() - 1].to_vec();
                        new_segments.push(suggestion);
                        vec![CodeAction {
                            title: format!("Did you mean `{suggestion}`?"),
                            replacement: new_segments.join("."),
                            start: prop_path.location.0,
                            end: prop_path.location.1,
                        }]
                    })
                    .unwrap_or_default()
            } else {
                vec![]
            };
            diagnostics.push(Diagnostic {
                message: format!(
                    "Property path `{}` is not defined in the schema",
                    prop_path.path
                ),
                severity: DiagnosticSeverity::Warning,
                start: prop_path.location.0,
                end: prop_path.location.1,
                code_actions,
            });
        }
    }
}

/// Check that transforms receive compatible input types.
fn check_transform_types(ast: &Expression, index: &SchemaIndex, diagnostics: &mut Vec<Diagnostic>) {
    let transforms = collect_transforms(ast);
    let registry = &*TRANSFORMER_REGISTRY;

    for call in &transforms {
        let Some(descriptor) = registry.get(call.name) else {
            let names: Vec<&str> = registry.keys().copied().collect();
            let code_actions = find_closest_match(call.name, &names, 2)
                .map(|suggestion| vec![CodeAction {
                    title: format!("Did you mean `{suggestion}`?"),
                    replacement: suggestion.to_string(),
                    start: call.location.0,
                    end: call.location.1,
                }])
                .unwrap_or_default();
            diagnostics.push(Diagnostic {
                message: format!("Unknown transform `{}`", call.name),
                severity: DiagnosticSeverity::Warning,
                start: call.location.0,
                end: call.location.1,
                code_actions,
            });
            continue;
        };

        // Check argument count and types
        check_transform_args(call, descriptor, index, diagnostics);

        // Skip transforms that accept any input
        if descriptor.input_types.contains(&PrimitiveType::Any) {
            continue;
        }

        // Try to infer the subject type — first via expression type inference,
        // then fall back to schema path resolution.
        let subject_types = infer_expr_types(call.subject, index);
        let subject_types = subject_types.or_else(|| {
            let path = resolve_dot_chain(call.subject);
            if path.is_empty() {
                None
            } else {
                types_for_path(&path, index)
            }
        });

        if let Some(subject_types) = subject_types {
            let compatible = subject_types.iter().any(|st| {
                st == &PrimitiveType::Any
                    || descriptor
                        .input_types
                        .iter()
                        .any(|it| it.compatible_with(*st))
            });

            if !compatible {
                let subject_type_str: Vec<String> = subject_types
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect();
                let expected_str: Vec<String> = descriptor
                    .input_types
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect();
                let subject_desc = resolve_dot_chain(call.subject);
                let subject_label = if subject_desc.is_empty() {
                    "expression".to_string()
                } else {
                    format!("`{subject_desc}`")
                };
                diagnostics.push(Diagnostic {
                    message: format!(
                        "Transform `{}` expects input of type {} but {} is {}",
                        call.name,
                        expected_str.join(" | "),
                        subject_label,
                        subject_type_str.join(" | "),
                    ),
                    severity: DiagnosticSeverity::Warning,
                    start: call.location.0,
                    end: call.location.1,
                    code_actions: vec![],
                });
            }
        }
    }
}

/// Check that a transform call has the right number and types of arguments.
fn check_transform_args(
    call: &TransformCall<'_>,
    descriptor: &TransformerDescriptor,
    index: &SchemaIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let required_count = descriptor.args.iter().filter(|a| a.required).count();
    let max_count = descriptor.args.len();

    if call.arg_count < required_count {
        let noun = if required_count == 1 {
            "argument"
        } else {
            "arguments"
        };
        diagnostics.push(Diagnostic {
            message: format!(
                "Transform `{}` requires {} {noun} but {} provided",
                call.name, required_count, call.arg_count,
            ),
            severity: DiagnosticSeverity::Error,
            start: call.location.0,
            end: call.location.1,
            code_actions: vec![],
        });
    } else if call.arg_count > max_count {
        diagnostics.push(Diagnostic {
            message: format!(
                "Transform `{}` accepts at most {} argument{} but {} provided",
                call.name,
                max_count,
                if max_count == 1 { "" } else { "s" },
                call.arg_count,
            ),
            severity: DiagnosticSeverity::Warning,
            start: call.location.0,
            end: call.location.1,
            code_actions: vec![],
        });
    }

    // Check argument types against the descriptor
    if let Some(arg_list) = call.args {
        for (i, arg_expr) in arg_list.iter().enumerate() {
            let Some(arg_desc) = descriptor.args.get(i) else {
                break; // beyond declared args — already warned about count
            };

            // Skip if descriptor accepts any type
            if arg_desc.types.contains(&PrimitiveType::Any) {
                continue;
            }

            if let Some(arg_types) = infer_expr_types(arg_expr, index) {
                let compatible = arg_types.iter().any(|at| {
                    at == &PrimitiveType::Any
                        || arg_desc.types.iter().any(|expected| expected.compatible_with(*at))
                });

                if !compatible {
                    let actual_str: Vec<String> =
                        arg_types.iter().map(std::string::ToString::to_string).collect();
                    let expected_str: Vec<String> =
                        arg_desc.types.iter().map(std::string::ToString::to_string).collect();
                    diagnostics.push(Diagnostic {
                        message: format!(
                            "Transform `{}` argument `{}` expects type {} but got {}",
                            call.name,
                            arg_desc.name,
                            expected_str.join(" | "),
                            actual_str.join(" | "),
                        ),
                        severity: DiagnosticSeverity::Warning,
                        start: arg_expr.location.0,
                        end: arg_expr.location.1,
                        code_actions: vec![],
                    });
                }
            }
        }
    }
}

/// Check binary operations for type compatibility.
fn check_binary_ops(ast: &Expression, index: &SchemaIndex, diagnostics: &mut Vec<Diagnostic>) {
    check_binary_ops_inner(ast, index, diagnostics);
}

fn check_binary_ops_inner(
    expr: &Expression,
    index: &SchemaIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expr::BinaryOperation {
        operation,
        left,
        right,
    } = &expr.expression
    {
        check_binary_op_types(*operation, left, right, expr.location, index, diagnostics);
    }

    // Recurse
    let mut visit = |child: &Expression| {
        check_binary_ops_inner(child, index, diagnostics);
    };
    super::ast_walk::visit_children_pub(expr, &mut visit);
}

fn check_binary_op_types(
    op: OpCode,
    left: &Expression,
    right: &Expression,
    location: (usize, usize),
    index: &SchemaIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let left_types = infer_expr_types(left, index);
    let right_types = infer_expr_types(right, index);

    // If we can't infer types, skip
    let (Some(left_types), Some(right_types)) = (left_types, right_types) else { return };

    match op {
        // Arithmetic ops require numeric operands
        OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide
        | OpCode::FloorDivide
        | OpCode::Modulus
        | OpCode::Exponent => {
            let left_numeric = left_types.iter().any(|t| {
                matches!(
                    t,
                    PrimitiveType::Number | PrimitiveType::Integer | PrimitiveType::Any
                )
            });
            let right_numeric = right_types.iter().any(|t| {
                matches!(
                    t,
                    PrimitiveType::Number | PrimitiveType::Integer | PrimitiveType::Any
                )
            });

            if !left_numeric || !right_numeric {
                diagnostics.push(Diagnostic {
                    message: format!("Arithmetic operation `{op}` requires numeric operands"),
                    severity: DiagnosticSeverity::Warning,
                    start: location.0,
                    end: location.1,
                    code_actions: vec![],
                });
            }
        }
        // Comparison ops require compatible orderable types
        OpCode::Less | OpCode::LessEqual | OpCode::Greater | OpCode::GreaterEqual => {
            let compatible = left_types
                .iter()
                .any(|lt| right_types.iter().any(|rt| lt.compatible_with(*rt)));
            if !compatible {
                diagnostics.push(Diagnostic {
                    message: format!("Comparison `{op}` between incompatible types"),
                    severity: DiagnosticSeverity::Warning,
                    start: location.0,
                    end: location.1,
                    code_actions: vec![],
                });
            }
        }
        // Regex ops require a string left-hand side
        OpCode::Matches | OpCode::Capture | OpCode::CaptureMultiple => {
            let left_string = left_types.iter().any(|t| {
                matches!(t, PrimitiveType::String | PrimitiveType::Any)
            });
            if !left_string {
                diagnostics.push(Diagnostic {
                    message: format!("Regex operation `{op}` requires a string left-hand side"),
                    severity: DiagnosticSeverity::Warning,
                    start: location.0,
                    end: location.1,
                    code_actions: vec![],
                });
            }
        }
        _ => {}
    }
}

/// Try to infer the types of an expression from the schema.
fn infer_expr_types(expr: &Expression, index: &SchemaIndex) -> Option<Vec<PrimitiveType>> {
    match &expr.expression {
        Expr::Number(_) => Some(vec![PrimitiveType::Number]),
        Expr::String(_) => Some(vec![PrimitiveType::String]),
        Expr::Boolean(_) => Some(vec![PrimitiveType::Boolean]),
        Expr::Null => Some(vec![PrimitiveType::Null]),
        Expr::Array(_) => Some(vec![PrimitiveType::Array]),
        Expr::Object(_) => Some(vec![PrimitiveType::Object]),
        Expr::Identifier(name) => types_for_path(name, index),
        Expr::DotOperation { .. } => {
            let path = resolve_schema_path(expr);
            types_for_path(&path, index).or_else(|| {
                let fallback = resolve_dot_chain(expr);
                types_for_path(&fallback, index)
            })
        }
        Expr::IndexOperation { .. } => {
            let path = resolve_schema_path(expr);
            types_for_path(&path, index)
        }
        Expr::Transform { name, subject, .. } => {
            infer_transform_output(name, subject, index)
        }
        Expr::Regex(_, _) => Some(vec![PrimitiveType::String]),
        Expr::BinaryOperation { operation, .. } => match operation {
            OpCode::Matches => Some(vec![PrimitiveType::Boolean]),
            OpCode::Capture | OpCode::CaptureMultiple => Some(vec![PrimitiveType::Array]),
            _ => None,
        },
        _ => None,
    }
}

/// Infer the output types of a transform, using `OutputTypeKind` to resolve
/// element types through array operations (e.g. `first`, `filter`, `sort`).
fn infer_transform_output(
    name: &str,
    subject: &Expression,
    index: &SchemaIndex,
) -> Option<Vec<PrimitiveType>> {
    let descriptor = TRANSFORMER_REGISTRY.get(name)?;
    match descriptor.output_type_kind {
        OutputTypeKind::Fixed => Some(descriptor.output_types.to_vec()),
        OutputTypeKind::ElementOfInput => {
            // Resolve subject's array element type from the schema
            if let Some(item_path) = resolve_transform_item_path_for_expr(subject) {
                if let Some(types) = types_for_path(&item_path, index) {
                    if !types.contains(&PrimitiveType::Any) {
                        return Some(types);
                    }
                }
            }
            // Fall back to static output types
            Some(descriptor.output_types.to_vec())
        }
        OutputTypeKind::ArrayPreservingElements => {
            // Output is still an array — return Array
            Some(vec![PrimitiveType::Array])
        }
        OutputTypeKind::LambdaDetermined => {
            // Cannot statically infer; use declared output types
            Some(descriptor.output_types.to_vec())
        }
    }
}

/// Resolve the `__arrayItem__` schema path for a transform's subject expression.
/// Walks through array-preserving transforms to find the underlying array path.
fn resolve_transform_item_path_for_expr(expr: &Expression) -> Option<String> {
    match &expr.expression {
        Expr::Identifier(_) | Expr::DotOperation { .. } => {
            let path = resolve_dot_chain(expr);
            if path.is_empty() {
                None
            } else {
                Some(format!("{path}.__arrayItem__"))
            }
        }
        Expr::Transform { name, subject, .. } => {
            let registry = &*TRANSFORMER_REGISTRY;
            let desc = registry.get(name.as_str())?;
            match desc.output_type_kind {
                // Array-preserving transforms pass through to the original array
                OutputTypeKind::ArrayPreservingElements => {
                    resolve_transform_item_path_for_expr(subject)
                }
                // LambdaDetermined (e.g. map) may still produce an array
                OutputTypeKind::LambdaDetermined
                    if desc.output_types.contains(&PrimitiveType::Array) =>
                {
                    resolve_transform_item_path_for_expr(subject)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Warn about nullable or optional properties used as transform subjects.
fn check_nullable_warnings(
    ast: &Expression,
    index: &SchemaIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let transforms = collect_transforms(ast);

    for call in &transforms {
        let Some((subject_path, entry)) = resolve_subject_to_path(call.subject, index) else { continue };

        if is_null_guarded(ast, &subject_path, call.location) {
            continue;
        }

        if entry.nullable {
            diagnostics.push(Diagnostic {
                message: format!(
                    "Property `{subject_path}` is nullable; `{}` may fail at runtime",
                    call.name
                ),
                severity: DiagnosticSeverity::Info,
                start: call.location.0,
                end: call.location.1,
                code_actions: vec![],
            });
        } else if !entry.required {
            diagnostics.push(Diagnostic {
                message: format!(
                    "Property `{subject_path}` is optional; `{}` may fail at runtime",
                    call.name
                ),
                severity: DiagnosticSeverity::Info,
                start: call.location.0,
                end: call.location.1,
                code_actions: vec![],
            });
        }
    }
}

/// Resolve a transform subject expression to its schema path and entry.
///
/// Tries a simple dot-chain first (`customer.email`), then falls back to resolving
/// through element-extracting transforms (`(movies | first).overview`).
fn resolve_subject_to_path<'a>(
    expr: &Expression,
    index: &'a SchemaIndex,
) -> Option<(String, &'a super::types::SchemaEntry)> {
    // Strategy 1: simple dot chain
    let simple = resolve_dot_chain(expr);
    if !simple.is_empty()
        && let Some(entry) = resolve_path(&simple, index) {
            return Some((simple, entry));
        }

    // Strategy 2: DotOperation on a transform result (e.g. `(movies | first).overview`)
    if let Expr::DotOperation { subject, ident } = &expr.expression
        && let Some(base_path) = resolve_transform_item_path(subject) {
            let full_path = format!("{base_path}.{ident}");
            if let Some(entry) = resolve_path(&full_path, index) {
                return Some((full_path, entry));
            }
        }

    None
}

/// Check whether a subject path is null-guarded in the enclosing expression.
///
/// Returns `true` when the transform lives inside the right branch of an `&&`
/// whose left operand resolves to the same path, e.g.:
///   `customer.email && customer.email | lowercase`
fn is_null_guarded(
    ast: &Expression,
    path: &str,
    transform_loc: (usize, usize),
) -> bool {
    find_null_guard(ast, path, transform_loc)
}

fn find_null_guard(expr: &Expression, path: &str, loc: (usize, usize)) -> bool {
    if let Expr::BinaryOperation {
            operation: OpCode::And,
            left,
            right,
        } = &expr.expression {
        let guard_path = resolve_dot_chain(left);
        if guard_path == path {
            // The transform is in the right subtree if its location falls within.
            let (start, end) = right.location;
            if loc.0 >= start && loc.1 <= end {
                return true;
            }
        }
        find_null_guard(left, path, loc) || find_null_guard(right, path, loc)
    } else {
        let mut found = false;
        visit_children_pub(expr, &mut |child| {
            if !found && find_null_guard(child, path, loc) {
                found = true;
            }
        });
        found
    }
}

/// Check `this.*` property paths inside lambda bodies against the array item schema.
///
/// For example, `items | map(this.bogus)` should warn if `items` is an array whose
/// item schema doesn't have a `bogus` property.
fn check_lambda_this_paths(
    ast: &Expression,
    index: &SchemaIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let lambda_paths = collect_lambda_paths(ast);

    for lp in &lambda_paths {
        // Resolve the array's item properties
        let item_props = resolve_array_item_properties(&lp.array_path, index);
        if item_props.is_empty() {
            // Can't determine item schema — skip
            continue;
        }

        // Check the root property of `this.foo.bar` → check `foo`
        let root_prop = lp.property.split('.').next().unwrap_or(&lp.property);
        if !item_props.iter().any(|(name, _)| name == root_prop) {
            let prop_candidates: Vec<&str> = item_props.iter().map(|(name, _)| name.as_str()).collect();
            let code_actions = find_closest_match(root_prop, &prop_candidates, 2)
                .map(|suggestion| {
                    let replacement = if lp.property.contains('.') {
                        let mut segs: Vec<&str> = lp.property.split('.').collect();
                        segs[0] = suggestion;
                        format!("this.{}", segs.join("."))
                    } else {
                        format!("this.{suggestion}")
                    };
                    vec![CodeAction {
                        title: format!("Did you mean `this.{suggestion}`?"),
                        replacement,
                        start: lp.location.0,
                        end: lp.location.1,
                    }]
                })
                .unwrap_or_default();
            diagnostics.push(Diagnostic {
                message: format!(
                    "Property `this.{}` is not defined on items of `{}`",
                    lp.property, lp.array_path
                ),
                severity: DiagnosticSeverity::Warning,
                start: lp.location.0,
                end: lp.location.1,
                code_actions,
            });
        }
    }
}

/// Compute the Levenshtein edit distance between two strings.
fn levenshtein(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();
    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j] + cost)
                .min(curr[j] + 1)
                .min(prev[j + 1] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b_len]
}

/// Find the closest match to `input` among `candidates` within `max_distance`.
fn find_closest_match<'a>(input: &str, candidates: &[&'a str], max_distance: usize) -> Option<&'a str> {
    let input_lower = input.to_lowercase();
    let mut best: Option<(&str, usize)> = None;
    for &candidate in candidates {
        let dist = levenshtein(&input_lower, &candidate.to_lowercase());
        if dist <= max_distance {
            if best.map_or(true, |(_, d)| dist < d) {
                best = Some((candidate, dist));
            }
        }
    }
    best.map(|(name, _)| name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_error() {
        let diags = validate("foo +", None, None);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, DiagnosticSeverity::Error);
        assert!(diags[0].message.contains("Parse error"));
    }

    #[test]
    fn test_undefined_property() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let diags = validate("unknown_prop", Some(&schema), None);
        assert!(diags.iter().any(|d| d.message.contains("not defined")));
    }

    #[test]
    fn test_known_property_no_error() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let diags = validate("name", Some(&schema), None);
        assert!(diags.is_empty());
    }

    #[test]
    fn test_transform_type_mismatch() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // uppercase expects string, but testing with a known string should be fine
        let diags = validate("name | uppercase", Some(&schema), None);
        assert!(
            diags.is_empty(),
            "uppercase on string should not warn: {diags:?}"
        );
    }

    #[test]
    fn test_nullable_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string", "nullable": true }
            }
        });
        let diags = validate("name | uppercase", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.severity == DiagnosticSeverity::Info && d.message.contains("nullable"))
        );
    }

    #[test]
    fn test_nullable_through_transform_chain() {
        // `(movies | first).overview` should warn when overview is nullable
        let schema = json!({
            "type": "object",
            "properties": {
                "movies": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "title": { "type": "string" },
                            "overview": { "type": "string", "nullable": true }
                        }
                    }
                }
            }
        });
        let diags = validate(
            "(movies | first).overview | lowercase",
            Some(&schema),
            None,
        );
        assert!(
            diags.iter().any(|d| d.severity == DiagnosticSeverity::Info
                && d.message.contains("nullable")
                && d.message.contains("overview")),
            "Expected nullable warning for overview through transform chain, got: {diags:?}"
        );
    }

    #[test]
    fn test_nullable_guard_suppresses_warning() {
        // `customer.email && customer.email | lowercase` should NOT warn
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "properties": {
                        "email": { "type": "string", "nullable": true }
                    }
                }
            }
        });
        let diags = validate(
            "customer.email && customer.email | lowercase",
            Some(&schema),
            None,
        );
        assert!(
            !diags
                .iter()
                .any(|d| d.severity == DiagnosticSeverity::Info && d.message.contains("nullable")),
            "Expected nullable warning to be suppressed by && guard, got: {diags:?}"
        );
    }

    #[test]
    fn test_nullable_without_guard_still_warns() {
        // `customer.email | lowercase` should warn when email is nullable
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": {
                    "type": "object",
                    "properties": {
                        "email": { "type": "string", "nullable": true }
                    }
                }
            }
        });
        let diags = validate("customer.email | lowercase", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.severity == DiagnosticSeverity::Info
                && d.message.contains("nullable")
                && d.message.contains("customer.email")),
            "Expected nullable warning for customer.email, got: {diags:?}"
        );
    }

    #[test]
    fn test_valid_expression_no_diagnostics() {
        let diags = validate("1 + 2", None, None);
        assert!(diags.is_empty());
    }

    #[test]
    fn test_lambda_body_unknown_var() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": { "type": "string" }
                        }
                    }
                }
            }
        });
        // `unknown_var` inside the reduce body should be caught
        let diags = validate("items | reduce(0, acc + unknown_var)", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("unknown_var")),
            "Should flag unknown_var inside lambda body, got: {diags:?}"
        );
    }

    #[test]
    fn test_lambda_this_property_valid() {
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
        // `this.name` is valid — should not produce a warning
        let diags = validate("items | map(this.name)", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("this.name")),
            "Should not flag valid this.name, got: {diags:?}"
        );
    }

    #[test]
    fn test_lambda_this_property_invalid() {
        let schema = json!({
            "type": "object",
            "properties": {
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": { "type": "string" }
                        }
                    }
                }
            }
        });
        // `this.bogus` is invalid — should produce a warning
        let diags = validate("items | map(this.bogus)", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("this.bogus")
                    && d.severity == DiagnosticSeverity::Warning),
            "Should flag undefined this.bogus, got: {diags:?}"
        );
    }

    #[test]
    fn test_lambda_this_property_filter() {
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
        // `this.active` in filter is valid
        let diags = validate("users | filter(this.active)", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("this.active")),
            "Should not flag valid this.active in filter, got: {diags:?}"
        );
        // `this.missing` in filter is invalid
        let diags = validate("users | filter(this.missing)", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("this.missing")),
            "Should flag undefined this.missing in filter, got: {diags:?}"
        );
    }

    #[test]
    fn test_transform_result_property_no_false_positive() {
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
        // `(movies | first).genre` should NOT produce a "not defined" error —
        // the subject is a transform, not a schema root.
        let diags = validate("(movies | first).genre", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("genre") && d.message.contains("not defined")),
            "Should not flag genre on transform result, got: {diags:?}"
        );
    }

    // --- Unknown transform warning ---

    #[test]
    fn test_unknown_transform_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        });
        let diags = validate("name | bogusTransform", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown transform")
                    && d.message.contains("bogusTransform")
                    && d.severity == DiagnosticSeverity::Warning),
            "Should flag unknown transform, got: {diags:?}"
        );
    }

    #[test]
    fn test_known_transform_no_unknown_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        let diags = validate("name | uppercase", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("Unknown transform")),
            "Should not flag known transform, got: {diags:?}"
        );
    }

    // --- Transform argument count validation ---

    #[test]
    fn test_transform_missing_required_args() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // `replace` requires 2 args (from, to)
        let diags = validate("name | replace", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("requires")
                    && d.message.contains("replace")
                    && d.severity == DiagnosticSeverity::Error),
            "Should flag missing required args, got: {diags:?}"
        );
    }

    #[test]
    fn test_transform_correct_args_no_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        let diags = validate("name | replace('a', 'b')", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("requires")
                || d.message.contains("at most")),
            "Should not flag correct arg count, got: {diags:?}"
        );
    }

    // --- Chained transform type propagation ---

    #[test]
    fn test_chained_transform_type_mismatch() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // split returns array, uppercase expects string
        let diags = validate("name | split(',') | uppercase", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("uppercase")
                    && d.message.contains("expects")
                    && d.severity == DiagnosticSeverity::Warning),
            "Should flag type mismatch through chain, got: {diags:?}"
        );
    }

    #[test]
    fn test_chained_transform_compatible_types() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // uppercase → string, then trim → string — both expect string
        let diags = validate("name | uppercase | trim", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("expects") && d.message.contains("trim")),
            "Should not flag compatible chain, got: {diags:?}"
        );
    }

    // --- Generic type tracking through array transforms ---

    #[test]
    fn test_element_of_input_first_string_array() {
        let schema = json!({
            "type": "object",
            "properties": {
                "names": {
                    "type": "array",
                    "items": { "type": "string" }
                }
            }
        });
        // first returns element type (string) from schema, uppercase expects string → ok
        let diags = validate("names | first | uppercase", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("uppercase") && d.message.contains("expects")),
            "Should not flag uppercase on string element from first, got: {diags:?}"
        );
    }

    #[test]
    fn test_element_of_input_first_number_array() {
        let schema = json!({
            "type": "object",
            "properties": {
                "scores": {
                    "type": "array",
                    "items": { "type": "number" }
                }
            }
        });
        // first returns number, uppercase expects string → should warn
        let diags = validate("scores | first | uppercase", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("uppercase") && d.message.contains("expects")),
            "Should flag uppercase on number element from first, got: {diags:?}"
        );
    }

    #[test]
    fn test_array_preserving_filter_then_first() {
        let schema = json!({
            "type": "object",
            "properties": {
                "names": {
                    "type": "array",
                    "items": { "type": "string" }
                }
            }
        });
        // filter preserves array element type, first extracts it → string
        let diags = validate("names | sort | first | uppercase", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("uppercase") && d.message.contains("expects")),
            "Should not flag uppercase on element from sort | first, got: {diags:?}"
        );
    }

    #[test]
    fn test_transform_arg_type_mismatch() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // replace expects (String, String) arguments; passing a number should warn
        let diags = validate("name | replace(42, 'b')", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("replace") && d.message.contains("argument")),
            "Should flag wrong argument type, got: {diags:?}"
        );
    }

    #[test]
    fn test_transform_arg_type_valid() {
        let schema = json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            },
            "required": ["name"]
        });
        // replace with string args → no warning
        let diags = validate("name | replace('a', 'b')", Some(&schema), None);
        assert!(
            !diags.iter().any(|d| d.message.contains("argument") && d.message.contains("type")),
            "Should not flag correct argument types, got: {diags:?}"
        );
    }

    // --- Code action / quick-fix tests ---

    #[test]
    fn test_code_action_unknown_transform_typo() {
        let schema = json!({
            "type": "object",
            "properties": { "name": { "type": "string" } }
        });
        let diags = validate("name | uppercse", Some(&schema), None);
        let d = diags.iter().find(|d| d.message.contains("Unknown transform")).unwrap();
        assert!(!d.code_actions.is_empty(), "Should have a code action for typo");
        assert!(d.code_actions[0].title.contains("uppercase"));
        assert_eq!(d.code_actions[0].replacement, "uppercase");
    }

    #[test]
    fn test_code_action_unknown_transform_no_match() {
        let schema = json!({
            "type": "object",
            "properties": { "name": { "type": "string" } }
        });
        let diags = validate("name | completelyWrong", Some(&schema), None);
        let d = diags.iter().find(|d| d.message.contains("Unknown transform")).unwrap();
        assert!(d.code_actions.is_empty(), "Should have no code action for distant name");
    }

    #[test]
    fn test_code_action_undefined_property_typo() {
        let schema = json!({
            "type": "object",
            "properties": {
                "customer": { "type": "object", "properties": { "name": { "type": "string" } } }
            }
        });
        let diags = validate("custmer", Some(&schema), None);
        let d = diags.iter().find(|d| d.message.contains("not defined")).unwrap();
        assert!(!d.code_actions.is_empty(), "Should suggest 'customer', got: {d:?}");
        assert!(d.code_actions[0].replacement.contains("customer"));
    }

    #[test]
    fn test_code_action_lambda_this_property_typo() {
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
        let diags = validate("items | map(this.nme)", Some(&schema), None);
        let d = diags.iter().find(|d| d.message.contains("not defined on items")).unwrap();
        assert!(!d.code_actions.is_empty(), "Should suggest 'name', got: {d:?}");
        assert!(d.code_actions[0].title.contains("name"));
        assert!(d.code_actions[0].replacement.contains("this.name"));
    }

    #[test]
    fn test_regex_match_on_string_no_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "email": { "type": "string" }
            },
            "required": ["email"]
        });
        let diags = validate("email ~ /test/", Some(&schema), None);
        assert!(
            diags.is_empty(),
            "regex match on string should not warn: {diags:?}"
        );
    }

    #[test]
    fn test_regex_match_on_number_warns() {
        let schema = json!({
            "type": "object",
            "properties": {
                "count": { "type": "number" }
            },
            "required": ["count"]
        });
        let diags = validate("count ~ /test/", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("requires a string")),
            "regex on number should warn about string LHS: {diags:?}"
        );
    }

    #[test]
    fn test_regex_capture_on_string_no_warning() {
        let schema = json!({
            "type": "object",
            "properties": {
                "email": { "type": "string" }
            },
            "required": ["email"]
        });
        let diags = validate("email @ /(\\w+)@(.+)/", Some(&schema), None);
        assert!(
            diags.is_empty(),
            "regex capture on string should not warn: {diags:?}"
        );
    }

    #[test]
    fn test_regex_capture_multiple_on_number_warns() {
        let schema = json!({
            "type": "object",
            "properties": {
                "count": { "type": "number" }
            },
            "required": ["count"]
        });
        let diags = validate("count @+ /test/", Some(&schema), None);
        assert!(
            diags.iter().any(|d| d.message.contains("requires a string")),
            "regex capture_multiple on number should warn: {diags:?}"
        );
    }

    #[test]
    fn test_regex_literal_expression_no_parse_error() {
        let diags = validate("\"test\" ~ /te*/", None, None);
        assert!(
            diags.is_empty(),
            "valid regex expression should have no errors: {diags:?}"
        );
    }

    // --- Array indexing type unwrapping ---

    fn movies_schema() -> Value {
        json!({
            "type": "object",
            "properties": {
                "movies": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "title": { "type": "string" },
                            "year": { "type": "integer" },
                            "genre": {
                                "type": "array",
                                "items": { "type": "string" }
                            }
                        }
                    }
                },
                "scores": {
                    "type": "array",
                    "items": { "type": "number" }
                }
            }
        })
    }

    #[test]
    fn test_index_unwraps_array_element_nested() {
        // movies[0].genre[0] | lowercase — genre[0] is string, no warning
        let schema = movies_schema();
        let diags = validate("movies[0].genre[0] | lowercase", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("lowercase") && d.message.contains("expects")),
            "Should not flag lowercase on string element from genre[0], got: {diags:?}"
        );
    }

    #[test]
    fn test_index_unwraps_array_element_property() {
        // movies[0].title | uppercase — title is string, no warning
        let schema = movies_schema();
        let diags = validate("movies[0].title | uppercase", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("uppercase") && d.message.contains("expects")),
            "Should not flag uppercase on string from movies[0].title, got: {diags:?}"
        );
    }

    #[test]
    fn test_index_unwraps_simple_array() {
        // scores[0] | round — scores[0] is number, no warning
        let schema = movies_schema();
        let diags = validate("scores[0] | round", Some(&schema), None);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("round") && d.message.contains("expects")),
            "Should not flag round on number from scores[0], got: {diags:?}"
        );
    }

    #[test]
    fn test_array_without_index_still_warns() {
        // movies[0].genre | lowercase — genre is still an array, should warn
        let schema = movies_schema();
        let diags = validate("movies[0].genre | lowercase", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("lowercase") && d.message.contains("expects")),
            "Should flag lowercase on array genre without indexing, got: {diags:?}"
        );
    }

    #[test]
    fn test_index_unwraps_number_rejects_string_transform() {
        // movies[0].year | lowercase — year is integer, should warn
        let schema = movies_schema();
        let diags = validate("movies[0].year | lowercase", Some(&schema), None);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("lowercase") && d.message.contains("expects")),
            "Should flag lowercase on integer from movies[0].year, got: {diags:?}"
        );
    }
}
