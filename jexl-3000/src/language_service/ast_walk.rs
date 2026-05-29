use jexl_parser::ast::{Expr, Expression, Location};
use serde_json::Value;

/// The kind of lambda enclosing a given position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LambdaKind {
    Map,
    Filter,
    SortBy,
    Any,
    All,
    Find,
    FindIndex,
    Reduce,
    /// The generic `ExpressionTransform` variant (flatMap, apply, etc.)
    ExpressionTransform,
}

/// Context about the lambda enclosing a given byte offset.
#[derive(Debug, Clone)]
pub struct LambdaContext {
    /// What kind of lambda this is.
    pub kind: LambdaKind,
    /// The dot-path of the array subject (e.g. `"orders"` in `orders | map(this.name)`).
    pub subject_path: String,
    /// For `reduce`, the init expression AST (to infer `acc` type).
    pub reduce_init: Option<Value>,
}

/// Find the enclosing lambda context for a given byte offset.
///
/// Walks the AST looking for map/filter/reduce/etc. expressions whose body
/// contains the offset, and returns the lambda kind + subject path.
#[must_use]
pub fn find_enclosing_lambda(ast: &Expression, offset: usize) -> Option<LambdaContext> {
    find_lambda_inner(ast, offset)
}

fn find_lambda_inner(expr: &Expression, offset: usize) -> Option<LambdaContext> {
    let (start, end) = expr.location;
    if offset < start || offset > end {
        return None;
    }

    match &expr.expression {
        Expr::MapTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::Map)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::FilterTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::Filter)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::SortByTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::SortBy)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::AnyTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::Any)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::AllTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::All)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::FindTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::Find)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::FindIndexTransform { subject, args, .. } => {
            if let Some(ctx) =
                find_in_lambda_args(args.as_deref(), offset, subject, LambdaKind::FindIndex)
            {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        Expr::ExpressionTransform {
            name,
            subject,
            expression,
            ..
        } => {
            let (es, ee) = expression.location;
            if offset >= es && offset <= ee {
                let kind = match name {
                    jexl_parser::ast::ExpressionTransform::Map => LambdaKind::Map,
                    jexl_parser::ast::ExpressionTransform::Filter => LambdaKind::Filter,
                    jexl_parser::ast::ExpressionTransform::SortBy => LambdaKind::SortBy,
                    jexl_parser::ast::ExpressionTransform::Any => LambdaKind::Any,
                    jexl_parser::ast::ExpressionTransform::All => LambdaKind::All,
                    jexl_parser::ast::ExpressionTransform::Find => LambdaKind::Find,
                    jexl_parser::ast::ExpressionTransform::FindIndex => LambdaKind::FindIndex,
                    jexl_parser::ast::ExpressionTransform::Apply => LambdaKind::ExpressionTransform,
                };
                return Some(LambdaContext {
                    kind,
                    subject_path: resolve_dot_chain(subject),
                    reduce_init: None,
                });
            }
            find_lambda_inner(subject, offset)
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            let (es, ee) = expression.location;
            if offset >= es && offset <= ee {
                let init_value = eval_literal_expr(init);
                return Some(LambdaContext {
                    kind: LambdaKind::Reduce,
                    subject_path: resolve_dot_chain(subject),
                    reduce_init: init_value,
                });
            }
            // Check if offset is in init or subject
            if let Some(ctx) = find_lambda_inner(init, offset) {
                return Some(ctx);
            }
            find_lambda_inner(subject, offset)
        }
        // For other nodes, recurse into children
        _ => {
            let mut result = None;
            visit_children_pub(expr, &mut |child| {
                if result.is_none() {
                    result = find_lambda_inner(child, offset);
                }
            });
            result
        }
    }
}

/// Check if the offset falls inside any of the lambda args.
fn find_in_lambda_args(
    args: Option<&[Box<Expression>]>,
    offset: usize,
    subject: &Expression,
    kind: LambdaKind,
) -> Option<LambdaContext> {
    if let Some(arg_list) = args {
        for arg in arg_list {
            let (as_, ae) = arg.location;
            if offset >= as_ && offset <= ae {
                return Some(LambdaContext {
                    kind,
                    subject_path: resolve_dot_chain(subject),
                    reduce_init: None,
                });
            }
            // Check for nested lambdas inside args
            if let Some(ctx) = find_lambda_inner(arg, offset) {
                return Some(ctx);
            }
        }
    }
    None
}

/// Try to extract a simple JSON literal value from an AST expression.
/// Used for inferring the type of `reduce` init values.
#[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
fn eval_literal_expr(expr: &Expression) -> Option<Value> {
    match &expr.expression {
        Expr::Number(n) => {
            // Prefer integer representation when the float is an exact integer
            if n.fract() == 0.0 && *n >= i64::MIN as f64 && *n <= i64::MAX as f64 {
                Some(Value::Number(serde_json::Number::from(*n as i64)))
            } else {
                Some(Value::Number(
                    serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
                ))
            }
        }
        Expr::String(s) => Some(Value::String(s.clone())),
        Expr::Boolean(b) => Some(Value::Bool(*b)),
        Expr::Null => Some(Value::Null),
        Expr::Array(items) => {
            let vals: Option<Vec<Value>> = items.iter().map(|i| eval_literal_expr(i)).collect();
            vals.map(Value::Array)
        }
        Expr::Object(pairs) => {
            let mut map = serde_json::Map::new();
            for (key, val_expr) in pairs {
                let val = eval_literal_expr(val_expr)?;
                map.insert(key.clone(), val);
            }
            Some(Value::Object(map))
        }
        _ => None,
    }
}

/// Information about a node found at a specific byte offset.
#[derive(Debug, Clone)]
pub enum NodeAtOffset<'a> {
    /// An identifier (root context variable).
    Identifier { name: &'a str, location: Location },
    /// A property in a dot-access chain: `subject.ident`.
    /// `full_path` is the reconstructed dot-path from root (e.g. `"customer.address.street"`).
    DotProperty {
        ident: &'a str,
        full_path: String,
        location: Location,
    },
    /// A transform name.
    Transform { name: &'a str, location: Location },
    /// A filter item property (`.field` in `arr[.field == ...]`).
    /// `array_context` is the dot-path of the enclosing array (e.g. `"order.items"`).
    FilterProperty {
        name: &'a str,
        location: Location,
        array_context: Option<String>,
    },
    /// A regex literal (`/pattern/flags`).
    Regex {
        pattern: &'a str,
        flags: &'a str,
        location: Location,
    },
    /// A literal or other expression node.
    Other { location: Location },
}

impl NodeAtOffset<'_> {
    #[must_use]
    pub const fn location(&self) -> Location {
        match self {
            NodeAtOffset::Identifier { location, .. }
            | NodeAtOffset::DotProperty { location, .. }
            | NodeAtOffset::Transform { location, .. }
            | NodeAtOffset::FilterProperty { location, .. }
            | NodeAtOffset::Regex { location, .. }
            | NodeAtOffset::Other { location } => *location,
        }
    }
}

/// Find the deepest AST node whose span contains the given byte offset.
#[must_use]
pub fn find_node_at_offset(ast: &Expression, offset: usize) -> Option<NodeAtOffset<'_>> {
    find_in_expr(ast, offset)
}

fn find_in_expr(expr: &Expression, offset: usize) -> Option<NodeAtOffset<'_>> {
    let (start, end) = expr.location;
    if offset < start || offset > end {
        return None;
    }

    // Try children first (deeper nodes preferred)
    if let Some(child) = find_in_children(expr, offset) {
        return Some(child);
    }

    // This node contains the offset but no child does — classify it
    Some(classify_node(expr))
}

fn find_in_children(expr: &Expression, offset: usize) -> Option<NodeAtOffset<'_>> {
    match &expr.expression {
        Expr::DotOperation { subject, ident } => {
            // Check if offset is on the ident part (after the dot)
            // The ident spans from (subject.end + 1) to expr.end approx
            let ident_start = expr.location.1 - ident.len();
            if offset >= ident_start && offset <= expr.location.1 {
                let full_path = if !dot_chain_has_identifier_root(expr)
                    && let Some(item_path) = resolve_transform_item_path(subject)
                {
                    format!("{item_path}.{ident}")
                } else {
                    resolve_dot_chain(expr)
                };
                return Some(NodeAtOffset::DotProperty {
                    ident,
                    full_path,
                    location: (ident_start, expr.location.1),
                });
            }
            find_in_expr(subject, offset)
        }
        Expr::Transform {
            name,
            subject,
            args,
        } => {
            // Check if offset is on the transform name
            // Transform name appears after the `|` — approximate: after subject's end
            let name_approx_start = subject.location.1;
            if offset > name_approx_start {
                // Check if we're in an argument
                if let Some(arg_list) = args {
                    for arg in arg_list {
                        if let Some(found) = find_in_expr(arg, offset) {
                            return Some(found);
                        }
                    }
                }
                return Some(NodeAtOffset::Transform {
                    name,
                    location: expr.location,
                });
            }
            find_in_expr(subject, offset)
        }
        Expr::ExpressionTransform {
            subject,
            expression,
            args: _,
            name: _,
        } => {
            if let Some(found) = find_in_expr(expression, offset) {
                return Some(found);
            }
            find_in_expr(subject, offset)
        }
        Expr::MapTransform { subject, args, .. }
        | Expr::FilterTransform { subject, args, .. }
        | Expr::SortByTransform { subject, args, .. }
        | Expr::AnyTransform { subject, args, .. }
        | Expr::AllTransform { subject, args, .. }
        | Expr::FindTransform { subject, args, .. }
        | Expr::FindIndexTransform { subject, args, .. } => {
            if let Some(arg_list) = args {
                for arg in arg_list {
                    if let Some(found) = find_in_expr(arg, offset) {
                        return Some(found);
                    }
                }
            }
            find_in_expr(subject, offset)
        }
        Expr::BinaryOperation { left, right, .. } => {
            find_in_expr(right, offset).or_else(|| find_in_expr(left, offset))
        }
        Expr::UnaryOperation { right, .. } => find_in_expr(right, offset),
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => find_in_expr(falsy, offset)
            .or_else(|| find_in_expr(truthy, offset))
            .or_else(|| find_in_expr(left, offset)),
        Expr::IndexOperation {
            subject,
            index,
            is_filter,
        } => {
            if *is_filter {
                // For filter expressions, propagate the array context path
                let array_path = resolve_dot_chain(subject);
                if let Some(found) = find_in_expr_with_filter_ctx(index, offset, &array_path) {
                    return Some(found);
                }
            } else if let Some(found) = find_in_expr(index, offset) {
                return Some(found);
            }
            find_in_expr(subject, offset)
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => find_in_expr(expression, offset)
            .or_else(|| find_in_expr(init, offset))
            .or_else(|| find_in_expr(subject, offset)),
        Expr::Array(items) => {
            for item in items {
                if let Some(found) = find_in_expr(item, offset) {
                    return Some(found);
                }
            }
            None
        }
        Expr::Object(pairs) => {
            for (_, val) in pairs {
                if let Some(found) = find_in_expr(val, offset) {
                    return Some(found);
                }
            }
            None
        }
        _ => None,
    }
}

/// Like `find_in_expr` but propagates array context for filter property resolution.
fn find_in_expr_with_filter_ctx<'a>(
    expr: &'a Expression,
    offset: usize,
    array_path: &str,
) -> Option<NodeAtOffset<'a>> {
    let (start, end) = expr.location;
    if offset < start || offset > end {
        return None;
    }

    // Check if this is a FilterItemProperty
    if let Expr::FilterItemProperty(name) = &expr.expression {
        return Some(NodeAtOffset::FilterProperty {
            name,
            location: expr.location,
            array_context: Some(array_path.to_string()),
        });
    }

    // Recurse into children, propagating the filter context
    match &expr.expression {
        Expr::BinaryOperation { left, right, .. } => {
            find_in_expr_with_filter_ctx(right, offset, array_path)
                .or_else(|| find_in_expr_with_filter_ctx(left, offset, array_path))
        }
        Expr::UnaryOperation { right, .. } => {
            find_in_expr_with_filter_ctx(right, offset, array_path)
        }
        Expr::DotOperation { subject, ident } => {
            let ident_start = expr.location.1 - ident.len();
            if offset >= ident_start && offset <= expr.location.1 {
                // A dot on a filter property like `.item.subfield`
                let full_path = resolve_dot_chain(expr);
                return Some(NodeAtOffset::DotProperty {
                    ident,
                    full_path,
                    location: (ident_start, expr.location.1),
                });
            }
            find_in_expr_with_filter_ctx(subject, offset, array_path)
        }
        _ => {
            // For other nodes, try the standard find
            find_in_expr(expr, offset)
        }
    }
}

fn classify_node(expr: &Expression) -> NodeAtOffset<'_> {
    match &expr.expression {
        Expr::Identifier(name) => NodeAtOffset::Identifier {
            name,
            location: expr.location,
        },
        Expr::Now => NodeAtOffset::Identifier {
            name: "$now",
            location: expr.location,
        },
        Expr::NowUtc => NodeAtOffset::Identifier {
            name: "$now_utc",
            location: expr.location,
        },
        Expr::FilterItemProperty(name) => NodeAtOffset::FilterProperty {
            name,
            location: expr.location,
            array_context: None,
        },
        // Lambda transforms: when the cursor is on the keyword itself (not in
        // any child expression), surface them as a named Transform so hover
        // can look them up in the registry.
        Expr::ExpressionTransform { name, .. } => {
            let transform_name: &'static str = match name {
                jexl_parser::ast::ExpressionTransform::Map => "map",
                jexl_parser::ast::ExpressionTransform::Filter => "filter",
                jexl_parser::ast::ExpressionTransform::SortBy => "sortBy",
                jexl_parser::ast::ExpressionTransform::Any => "any",
                jexl_parser::ast::ExpressionTransform::All => "all",
                jexl_parser::ast::ExpressionTransform::Find => "find",
                jexl_parser::ast::ExpressionTransform::FindIndex => "findIndex",
                jexl_parser::ast::ExpressionTransform::Apply => "apply",
            };
            NodeAtOffset::Transform {
                name: transform_name,
                location: expr.location,
            }
        }
        Expr::MapTransform { name, .. }
        | Expr::FilterTransform { name, .. }
        | Expr::SortByTransform { name, .. }
        | Expr::AnyTransform { name, .. }
        | Expr::AllTransform { name, .. }
        | Expr::FindTransform { name, .. }
        | Expr::FindIndexTransform { name, .. } => NodeAtOffset::Transform {
            name,
            location: expr.location,
        },
        Expr::ReduceExpression { .. } => NodeAtOffset::Transform {
            name: "reduce",
            location: expr.location,
        },
        Expr::Regex(pattern, flags) => NodeAtOffset::Regex {
            pattern,
            flags,
            location: expr.location,
        },
        _ => NodeAtOffset::Other {
            location: expr.location,
        },
    }
}

/// Reconstruct the full dot-path for a `DotOperation` chain.
/// E.g. `DotOp(DotOp(Identifier("a"), "b"), "c")` → `"a.b.c"`
#[must_use]
pub fn resolve_dot_chain(expr: &Expression) -> String {
    let mut parts = Vec::new();
    collect_dot_parts(expr, &mut parts);
    parts.join(".")
}

fn collect_dot_parts<'a>(expr: &'a Expression, parts: &mut Vec<&'a str>) {
    match &expr.expression {
        Expr::DotOperation { subject, ident } => {
            collect_dot_parts(subject, parts);
            parts.push(ident);
        }
        Expr::IndexOperation { subject, .. } => {
            // Treat `arr[N]` transparently — just continue the chain from subject.
            collect_dot_parts(subject, parts);
        }
        Expr::Identifier(name) => {
            parts.push(name);
        }
        _ => {
            // Can't resolve further
        }
    }
}

/// Reconstruct a schema-aware dot-path that inserts `__arrayItem__` for each
/// non-filter `IndexOperation`, so that `movies[0].genre[0]` maps to
/// `"movies.__arrayItem__.genre.__arrayItem__"` for schema lookup.
#[must_use]
pub fn resolve_schema_path(expr: &Expression) -> String {
    let mut parts: Vec<String> = Vec::new();
    collect_schema_parts(expr, &mut parts);
    parts.join(".")
}

fn collect_schema_parts(expr: &Expression, parts: &mut Vec<String>) {
    match &expr.expression {
        Expr::DotOperation { subject, ident } => {
            collect_schema_parts(subject, parts);
            parts.push(ident.clone());
        }
        Expr::IndexOperation {
            subject, is_filter, ..
        } => {
            collect_schema_parts(subject, parts);
            if !is_filter {
                // Numeric index like `arr[0]` — unwrap the array element type.
                parts.push("__arrayItem__".to_string());
            }
        }
        Expr::Identifier(name) => {
            parts.push(name.clone());
        }
        _ => {}
    }
}

/// Resolve the effective array-item schema path for an expression that extracts
/// a single element from a typed array via a transform like `first` or `last`.
///
/// For example:
///   `movies | first`          → Some("movies.__arrayItem__")
///   `movies | sort | first`   → Some("movies.__arrayItem__")
///   `(movies | first)`        → Some("movies.__arrayItem__")  (unwraps parens)
///   `movies`                  → None  (not an element-extract)
///
/// This is used to provide hover/completion on `(array | first).property`.
#[must_use]
pub fn resolve_transform_item_path(expr: &Expression) -> Option<String> {
    use super::registry::{OutputTypeKind, TRANSFORMER_REGISTRY};

    match &expr.expression {
        Expr::Transform { name, subject, .. } => {
            let registry = &*TRANSFORMER_REGISTRY;
            let desc = registry.get(name.as_str())?;
            match desc.output_type_kind {
                OutputTypeKind::ElementOfInput => {
                    // The transform extracts an element — resolve the subject's array path.
                    let array_path = resolve_array_source_path(subject);
                    Some(format!("{}.__arrayItem__", array_path?))
                }
                OutputTypeKind::ArrayPreservingElements => {
                    // sort, reverse, etc. — still an array, not an element
                    None
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Walk through array-preserving transforms to find the underlying array path.
///   `movies`                → Some("movies")
///   `movies | sort`         → Some("movies")
///   `movies | sort | reverse` → Some("movies")
fn resolve_array_source_path(expr: &Expression) -> Option<String> {
    use super::registry::{OutputTypeKind, TRANSFORMER_REGISTRY};

    match &expr.expression {
        Expr::Identifier(_) | Expr::DotOperation { .. } => {
            let path = resolve_dot_chain(expr);
            if path.is_empty() { None } else { Some(path) }
        }
        Expr::Transform { name, subject, .. } => {
            let registry = &*TRANSFORMER_REGISTRY;
            let desc = registry.get(name.as_str())?;
            match desc.output_type_kind {
                OutputTypeKind::ArrayPreservingElements | OutputTypeKind::LambdaDetermined => {
                    resolve_array_source_path(subject)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Collected property path with its location span.
#[derive(Debug, Clone)]
pub struct PropertyPath {
    pub path: String,
    pub location: Location,
}

/// A `this.*` property path found inside a lambda body.
#[derive(Debug, Clone)]
pub struct LambdaPath {
    /// The `this`-relative path, e.g. `"name"` for `this.name`.
    pub property: String,
    /// Dot-path of the array subject, e.g. `"orders"` for `orders | map(this.name)`.
    pub array_path: String,
    pub location: Location,
}

/// Collected transform call info.
#[derive(Debug, Clone)]
pub struct TransformCall<'a> {
    pub name: &'a str,
    pub subject: &'a Expression,
    pub location: Location,
    /// Number of arguments passed to this transform (0 if no parens).
    pub arg_count: usize,
    /// The actual argument expressions, if any.
    pub args: Option<&'a Vec<Box<Expression>>>,
}

/// Walk the AST and collect all identifier/dot-access property paths.
#[must_use]
pub fn collect_all_paths(ast: &Expression) -> Vec<PropertyPath> {
    let mut paths = Vec::new();
    collect_paths_inner(ast, &mut paths);
    paths
}

/// Walk the AST and collect `this.*` property paths found inside lambda bodies,
/// paired with their array-subject path so the validator can check them against
/// the array's item schema.
#[must_use]
pub fn collect_lambda_paths(ast: &Expression) -> Vec<LambdaPath> {
    let mut results = Vec::new();
    collect_lambda_paths_inner(ast, &mut results);
    results
}

fn collect_lambda_paths_inner(expr: &Expression, results: &mut Vec<LambdaPath>) {
    match &expr.expression {
        // Expression transforms with lambda args: map, filter, sortBy, any, all, find, findIndex
        Expr::MapTransform { subject, args, .. }
        | Expr::FilterTransform { subject, args, .. }
        | Expr::SortByTransform { subject, args, .. }
        | Expr::AnyTransform { subject, args, .. }
        | Expr::AllTransform { subject, args, .. }
        | Expr::FindTransform { subject, args, .. }
        | Expr::FindIndexTransform { subject, args, .. } => {
            let array_path = resolve_dot_chain(subject);
            if let Some(arg_list) = args {
                for arg in arg_list {
                    collect_this_paths(arg, &array_path, results);
                }
            }
            // Recurse into subject for nested lambdas
            collect_lambda_paths_inner(subject, results);
        }
        // Reduce: body expression has `this` and `acc`
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            let array_path = resolve_dot_chain(subject);
            collect_this_paths(expression, &array_path, results);
            collect_lambda_paths_inner(subject, results);
            collect_lambda_paths_inner(init, results);
        }
        // ExpressionTransform (flatMap, etc.)
        Expr::ExpressionTransform {
            subject,
            expression,
            ..
        } => {
            let array_path = resolve_dot_chain(subject);
            collect_this_paths(expression, &array_path, results);
            collect_lambda_paths_inner(subject, results);
        }
        _ => {
            visit_children(expr, |child| collect_lambda_paths_inner(child, results));
        }
    }
}

/// Walk an expression looking for `this.*` dot-paths and emit them as `LambdaPath`.
fn collect_this_paths(expr: &Expression, array_path: &str, results: &mut Vec<LambdaPath>) {
    match &expr.expression {
        Expr::DotOperation { .. } => {
            let full = resolve_dot_chain(expr);
            if let Some(rest) = full.strip_prefix("this.") {
                results.push(LambdaPath {
                    property: rest.to_string(),
                    array_path: array_path.to_string(),
                    location: expr.location,
                });
            }
            // Recurse into non-this sub-expressions
            collect_this_inner_children(expr, array_path, results);
        }
        _ => {
            visit_children(expr, |child| collect_this_paths(child, array_path, results));
        }
    }
}

/// Recurse into children of a `DotOperation` without re-emitting intermediate dots.
fn collect_this_inner_children(expr: &Expression, array_path: &str, results: &mut Vec<LambdaPath>) {
    match &expr.expression {
        Expr::DotOperation { subject, .. } => {
            collect_this_inner_children(subject, array_path, results);
        }
        Expr::Identifier(_) => {} // part of the chain
        _ => {
            collect_this_paths(expr, array_path, results);
        }
    }
}

fn collect_paths_inner(expr: &Expression, paths: &mut Vec<PropertyPath>) {
    match &expr.expression {
        Expr::Identifier(name) => {
            paths.push(PropertyPath {
                path: name.clone(),
                location: expr.location,
            });
        }
        Expr::DotOperation { subject, .. } => {
            // Only emit the outermost dot chain (full path), not intermediate segments
            if !is_dot_child(expr) {
                // Only emit if the chain root is an identifier (not a transform,
                // conditional, etc.)  Otherwise the resolved path is partial and
                // would produce false-positive "not defined" errors.
                if dot_chain_has_identifier_root(expr) {
                    paths.push(PropertyPath {
                        path: resolve_dot_chain(expr),
                        location: expr.location,
                    });
                }
            }
            // Still recurse into subject in case it has non-dot sub-expressions
            collect_non_dot_children(subject, paths);
        }
        Expr::FilterItemProperty(_) => {
            // Skip — these are `.field` references inside `[.field == ...]`
        }
        _ => {
            // Recurse into all children
            visit_children(expr, |child| collect_paths_inner(child, paths));
        }
    }
}

/// For a `DotOperation` chain, only recurse into non-dot sub-parts.
fn collect_non_dot_children(expr: &Expression, paths: &mut Vec<PropertyPath>) {
    match &expr.expression {
        Expr::DotOperation { subject, .. } => {
            collect_non_dot_children(subject, paths);
        }
        Expr::Identifier(_) => {
            // Part of the dot chain — skip it to avoid double-counting
        }
        _ => {
            collect_paths_inner(expr, paths);
        }
    }
}

/// Check if this node is used as the subject of a parent `DotOperation`.
/// Since we don't have parent pointers, we approximate by checking that the
/// caller already collected the outer chain.
const fn is_dot_child(_expr: &Expression) -> bool {
    // This function is not usable without parent info.
    // Instead, we handle this in collect_paths_inner by only emitting
    // paths for the outermost DotOperation.
    false
}

/// Walk a `DotOperation` chain to its root and check that it ends at an
/// `Identifier` (or `IndexOperation` wrapping one).  Returns `false` when the
/// chain root is a transform, conditional, grouped expression, etc. — in that
/// case the resolved path would be partial and should not be schema-validated.
fn dot_chain_has_identifier_root(expr: &Expression) -> bool {
    match &expr.expression {
        Expr::DotOperation { subject, .. } | Expr::IndexOperation { subject, .. } => {
            dot_chain_has_identifier_root(subject)
        }
        Expr::Identifier(_) => true,
        _ => false,
    }
}

/// Walk the AST and collect all transform calls.
#[must_use]
pub fn collect_transforms(ast: &Expression) -> Vec<TransformCall<'_>> {
    let mut calls = Vec::new();
    collect_transforms_inner(ast, &mut calls);
    calls
}

fn collect_transforms_inner<'a>(expr: &'a Expression, calls: &mut Vec<TransformCall<'a>>) {
    match &expr.expression {
        Expr::Transform {
            name,
            subject,
            args,
        } => {
            calls.push(TransformCall {
                name,
                subject,
                location: expr.location,
                arg_count: args.as_ref().map_or(0, Vec::len),
                args: args.as_ref(),
            });
            collect_transforms_inner(subject, calls);
            if let Some(arg_list) = args {
                for arg in arg_list {
                    collect_transforms_inner(arg, calls);
                }
            }
        }
        Expr::ExpressionTransform {
            subject,
            expression,
            ..
        } => {
            collect_transforms_inner(subject, calls);
            collect_transforms_inner(expression, calls);
        }
        Expr::MapTransform { subject, args, .. }
        | Expr::FilterTransform { subject, args, .. }
        | Expr::SortByTransform { subject, args, .. }
        | Expr::AnyTransform { subject, args, .. }
        | Expr::AllTransform { subject, args, .. }
        | Expr::FindTransform { subject, args, .. }
        | Expr::FindIndexTransform { subject, args, .. } => {
            collect_transforms_inner(subject, calls);
            if let Some(arg_list) = args {
                for arg in arg_list {
                    collect_transforms_inner(arg, calls);
                }
            }
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            collect_transforms_inner(subject, calls);
            collect_transforms_inner(init, calls);
            collect_transforms_inner(expression, calls);
        }
        Expr::BinaryOperation { left, right, .. } => {
            collect_transforms_inner(left, calls);
            collect_transforms_inner(right, calls);
        }
        Expr::UnaryOperation { right, .. } => {
            collect_transforms_inner(right, calls);
        }
        Expr::DotOperation { subject, .. } => {
            collect_transforms_inner(subject, calls);
        }
        Expr::IndexOperation { subject, index, .. } => {
            collect_transforms_inner(subject, calls);
            collect_transforms_inner(index, calls);
        }
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            collect_transforms_inner(left, calls);
            collect_transforms_inner(truthy, calls);
            collect_transforms_inner(falsy, calls);
        }
        Expr::Array(items) => {
            for item in items {
                collect_transforms_inner(item, calls);
            }
        }
        Expr::Object(pairs) => {
            for (_, val) in pairs {
                collect_transforms_inner(val, calls);
            }
        }
        _ => {}
    }
}

/// Visit all direct child expressions of a node.
fn visit_children<F: FnMut(&Expression)>(expr: &Expression, mut f: F) {
    visit_children_pub(expr, &mut f);
}

/// Visit all direct child expressions of a node (public version).
pub fn visit_children_pub<F: FnMut(&Expression)>(expr: &Expression, f: &mut F) {
    match &expr.expression {
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Boolean(_)
        | Expr::Null
        | Expr::Now
        | Expr::NowUtc
        | Expr::Identifier(_)
        | Expr::FilterItemProperty(_)
        | Expr::Regex(_, _) => {}

        Expr::UnaryOperation { right, .. } => f(right),
        Expr::BinaryOperation { left, right, .. } => {
            f(left);
            f(right);
        }
        Expr::DotOperation { subject, .. } => f(subject),
        Expr::IndexOperation { subject, index, .. } => {
            f(subject);
            f(index);
        }
        Expr::Transform { subject, args, .. }
        | Expr::MapTransform { subject, args, .. }
        | Expr::FilterTransform { subject, args, .. }
        | Expr::SortByTransform { subject, args, .. }
        | Expr::AnyTransform { subject, args, .. }
        | Expr::AllTransform { subject, args, .. }
        | Expr::FindTransform { subject, args, .. }
        | Expr::FindIndexTransform { subject, args, .. } => {
            f(subject);
            if let Some(arg_list) = args {
                for arg in arg_list {
                    f(arg);
                }
            }
        }
        Expr::ExpressionTransform {
            subject,
            expression,
            ..
        } => {
            f(subject);
            f(expression);
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            f(subject);
            f(init);
            f(expression);
        }
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            f(left);
            f(truthy);
            f(falsy);
        }
        Expr::Array(items) => {
            for item in items {
                f(item);
            }
        }
        Expr::Object(pairs) => {
            for (_, val) in pairs {
                f(val);
            }
        }
    }
}
