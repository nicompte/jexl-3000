use jexl_parser::Parser;
use jexl_parser::ast::{Expr, ExpressionTransform, Expression, OpCode, UnCode};
use std::collections::BTreeMap;

const LINE_WIDTH: usize = 80;

/// A comment extracted from the source: `(byte_offset_of_#, text_without_#)`.
#[derive(Debug, Clone)]
struct Comment {
    /// Byte offset of the `#` character in the original source.
    offset: usize,
    /// The comment text (including the leading `# `).
    text: String,
}

/// Map from an AST node start-byte to the comments that precede it.
/// Uses BTreeMap so we can efficiently grab trailing comments (those after all nodes).
type CommentMap = BTreeMap<usize, Vec<String>>;

/// Extract `#`-style line comments from the source.
fn extract_comments(src: &str) -> Vec<Comment> {
    let mut comments = Vec::new();
    let bytes = src.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'#' {
            let start = i;
            // Scan to end of line
            while i < bytes.len() && bytes[i] != b'\n' && bytes[i] != b'\r' {
                i += 1;
            }
            let text = src[start..i].to_string();
            comments.push(Comment {
                offset: start,
                text,
            });
        } else if bytes[i] == b'\'' {
            // Skip single-quoted strings so we don't pick up `#` inside them
            i += 1;
            while i < bytes.len() && bytes[i] != b'\'' {
                if bytes[i] == b'\\' {
                    i += 1; // skip escaped char
                }
                i += 1;
            }
            if i < bytes.len() {
                i += 1; // closing quote
            }
        } else if bytes[i] == b'"' {
            // Skip double-quoted strings
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            if i < bytes.len() {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
    comments
}

/// Collect all AST node start-byte positions (recursively).
fn collect_node_starts(expr: &Expression, out: &mut Vec<usize>) {
    out.push(expr.location.0);
    match &expr.expression {
        Expr::Array(items) => {
            for item in items {
                collect_node_starts(item, out);
            }
        }
        Expr::Object(fields) => {
            for (_, val) in fields {
                collect_node_starts(val, out);
            }
        }
        Expr::UnaryOperation { right, .. } => collect_node_starts(right, out),
        Expr::BinaryOperation { left, right, .. } => {
            collect_node_starts(left, out);
            collect_node_starts(right, out);
        }
        Expr::Transform {
            subject, args, ..
        }
        | Expr::MapTransform {
            subject, args, ..
        }
        | Expr::SortByTransform {
            subject, args, ..
        }
        | Expr::AnyTransform {
            subject, args, ..
        }
        | Expr::AllTransform {
            subject, args, ..
        }
        | Expr::FindTransform {
            subject, args, ..
        }
        | Expr::FindIndexTransform {
            subject, args, ..
        }
        | Expr::FilterTransform {
            subject, args, ..
        } => {
            collect_node_starts(subject, out);
            if let Some(args) = args {
                for a in args {
                    collect_node_starts(a, out);
                }
            }
        }
        Expr::ExpressionTransform {
            subject,
            expression,
            ..
        } => {
            collect_node_starts(subject, out);
            collect_node_starts(expression, out);
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            collect_node_starts(subject, out);
            collect_node_starts(init, out);
            collect_node_starts(expression, out);
        }
        Expr::DotOperation { subject, .. } => collect_node_starts(subject, out),
        Expr::IndexOperation {
            subject, index, ..
        } => {
            collect_node_starts(subject, out);
            collect_node_starts(index, out);
        }
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            collect_node_starts(left, out);
            collect_node_starts(truthy, out);
            collect_node_starts(falsy, out);
        }
        _ => {}
    }
}

/// Build a map from AST node start positions to comments that immediately
/// precede them in the source.  Comments at the very end (after all nodes) are
/// stored under the key `usize::MAX`.
fn build_comment_map(comments: &[Comment], ast: &Expression) -> CommentMap {
    if comments.is_empty() {
        return CommentMap::new();
    }
    let mut starts = Vec::new();
    collect_node_starts(ast, &mut starts);
    starts.sort_unstable();
    starts.dedup();

    let mut map = CommentMap::new();
    for comment in comments {
        // Find the first AST node whose start position is > comment.offset
        let target = starts
            .iter()
            .find(|&&s| s > comment.offset)
            .copied()
            .unwrap_or(usize::MAX);
        map.entry(target)
            .or_default()
            .push(comment.text.clone());
    }
    map
}

/// Drain comments associated with the given node start position.
fn take_comments(cmap: &mut CommentMap, node_start: usize) -> Vec<String> {
    cmap.remove(&node_start).unwrap_or_default()
}

/// Format comments as lines at the given indentation, followed by a newline.
fn format_comments(comments: &[String], indent: usize) -> String {
    if comments.is_empty() {
        return String::new();
    }
    let prefix = " ".repeat(indent);
    comments
        .iter()
        .map(|c| format!("{prefix}{c}\n"))
        .collect()
}

/// Format a JEXL expression by parsing it and emitting a pretty-printed form.
///
/// Returns `Ok(formatted)` on success, or `Err(message)` if the expression
/// cannot be parsed.
pub fn format(expr: &str) -> Result<String, String> {
    let ast = Parser::parse(expr).map_err(|e| format!("Parse error: {e}"))?;
    let comments = extract_comments(expr);
    let mut cmap = build_comment_map(&comments, &ast);
    let has_comments = !comments.is_empty();

    let chain = collect_pipe_chain(&ast);
    // Break pipe chains with 3+ segments into multiline, or when flat form is long.
    if chain.len() >= 3 {
        let mut result = format_pipe_chain_commented(&chain, 0, &comments, 0);
        append_trailing_comments(&mut result, &mut cmap, 0);
        return Ok(result);
    }
    let flat = emit(&ast, 0);
    if flat.len() <= LINE_WIDTH && !has_comments {
        Ok(flat)
    } else {
        let mut result = emit_multiline_commented(&ast, 0, &mut cmap, &comments);
        append_trailing_comments(&mut result, &mut cmap, 0);
        Ok(result)
    }
}

/// Append any remaining trailing comments (stored under usize::MAX).
fn append_trailing_comments(output: &mut String, cmap: &mut CommentMap, indent: usize) {
    let trailing = take_comments(cmap, usize::MAX);
    if !trailing.is_empty() {
        for c in &trailing {
            let prefix = " ".repeat(indent);
            output.push('\n');
            output.push_str(&prefix);
            output.push_str(c);
        }
    }
}

/// Emit a single-line (flat) representation of the expression.
fn emit(expr: &Expression, _depth: usize) -> String {
    match &expr.expression {
        Expr::Number(n) => format_number(*n),
        Expr::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
        Expr::Boolean(b) => b.to_string(),
        Expr::Null => "null".to_string(),
        Expr::Now => "$now".to_string(),
        Expr::NowUtc => "$nowUTC".to_string(),
        Expr::Identifier(name) => name.clone(),
        Expr::Regex(pattern, flags) => format!("/{pattern}/{flags}"),

        Expr::Array(items) => {
            let parts: Vec<String> = items.iter().map(|i| emit(i, 0)).collect();
            format!("[{}]", parts.join(", "))
        }

        Expr::Object(fields) => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(key, val)| format!("{key}: {}", emit(val, 0)))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }

        Expr::UnaryOperation { operation, right } => {
            let op = match operation {
                UnCode::Not => "!",
                UnCode::Plus => "+",
                UnCode::Minus => "-",
            };
            let right_str = emit(right, 0);
            // Unary operand is ExprUnary in the grammar (prec >= 7).
            // Wrap binary ops and conditionals (prec < 7).
            if expr_precedence(&right.expression) < 7 {
                format!("{op}({right_str})")
            } else {
                format!("{op}{right_str}")
            }
        }

        Expr::BinaryOperation {
            operation,
            left,
            right,
        } => {
            let op = op_to_str(operation);
            let my_prec = op_precedence(operation);
            let left_str = emit(left, 0);
            let right_str = emit(right, 0);
            // Left: parens if child has strictly lower precedence
            let left_out = if expr_precedence(&left.expression) < my_prec {
                format!("({left_str})")
            } else {
                left_str
            };
            // Right: parens if child has lower or equal precedence (left-assoc)
            let right_out = if expr_precedence(&right.expression) <= my_prec {
                format!("({right_str})")
            } else {
                right_str
            };
            format!("{left_out} {op} {right_out}")
        }

        Expr::Transform {
            name,
            subject,
            args,
        } => format_transform(&emit(subject, 0), name, args),

        Expr::DotOperation { subject, ident } => {
            let sub = emit(subject, 0);
            if needs_parens(&subject.expression) {
                format!("({sub}).{ident}")
            } else {
                format!("{sub}.{ident}")
            }
        }

        Expr::IndexOperation {
            subject,
            index,
            is_filter: _,
        } => {
            let sub = emit(subject, 0);
            let idx = emit(index, 0);
            if needs_parens(&subject.expression) {
                format!("({sub})[{idx}]")
            } else {
                format!("{sub}[{idx}]")
            }
        }

        Expr::FilterItemProperty(prop) => format!(".{prop}"),

        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            let left_str = emit(left, 0);
            // Condition slot is ExprOr in the grammar; a nested Conditional needs parens
            let left_out = if matches!(left.expression, Expr::Conditional { .. }) {
                format!("({left_str})")
            } else {
                left_str
            };
            format!(
                "{left_out} ? {} : {}",
                emit(truthy, 0),
                emit(falsy, 0)
            )
        }

        Expr::MapTransform {
            subject,
            name,
            args,
        }
        | Expr::SortByTransform {
            subject,
            name,
            args,
        }
        | Expr::AnyTransform {
            subject,
            name,
            args,
        }
        | Expr::AllTransform {
            subject,
            name,
            args,
        }
        | Expr::FindTransform {
            subject,
            name,
            args,
        }
        | Expr::FindIndexTransform {
            subject,
            name,
            args,
        }
        | Expr::FilterTransform {
            subject,
            name,
            args,
        } => format_transform(&emit(subject, 0), name, args),

        Expr::ExpressionTransform {
            name,
            subject,
            expression,
            args,
        } => {
            let transform_name = match name {
                ExpressionTransform::Map => "map",
                ExpressionTransform::Filter => "filter",
                ExpressionTransform::SortBy => "sortBy",
                ExpressionTransform::Any => "any",
                ExpressionTransform::All => "all",
                ExpressionTransform::Find => "find",
                ExpressionTransform::FindIndex => "findIndex",
                ExpressionTransform::Apply => "apply",
            };
            let expr_str = emit(expression, 0);
            let args_str = if let Some(n) = args {
                format!(", {}", format_number(*n))
            } else {
                String::new()
            };
            format!(
                "{} | {transform_name}({}{args_str})",
                emit(subject, 0),
                expr_str
            )
        }

        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            format!(
                "{} | reduce({}, {})",
                emit(subject, 0),
                emit(init, 0),
                emit(expression, 0)
            )
        }
    }
}

/// Like `emit_or_multiline` but inserts leading comments from the map.
fn emit_or_multiline_commented(
    expr: &Expression,
    indent: usize,
    cmap: &mut CommentMap,
    comments: &[Comment],
) -> String {
    let flat = emit(expr, indent);
    let has_comments = cmap.keys().any(|&k| {
        k != usize::MAX && k >= expr.location.0 && k <= expr.location.1
    });
    if flat.len() + indent <= LINE_WIDTH && !has_comments {
        flat
    } else {
        emit_multiline_commented(expr, indent, cmap, comments)
    }
}

/// Emit a multi-line representation with comments from the map.
fn emit_multiline_commented(
    expr: &Expression,
    indent: usize,
    cmap: &mut CommentMap,
    comments: &[Comment],
) -> String {
    // Collect a pipe chain if this expression is one
    let chain = collect_pipe_chain(expr);
    if chain.len() > 1 {
        return format_pipe_chain_commented(&chain, indent, comments, expr.location.0);
    }

    match &expr.expression {
        Expr::Conditional {
            left,
            truthy,
            falsy,
        } => {
            let prefix = " ".repeat(indent);
            let cond = emit_or_multiline_commented(left, indent, cmap, comments);
            let cond_out = if matches!(left.expression, Expr::Conditional { .. }) {
                format!("({cond})")
            } else {
                cond
            };
            let t = emit_or_multiline_commented(truthy, indent + 2, cmap, comments);
            let f = emit_or_multiline_commented(falsy, indent + 2, cmap, comments);
            format!("{cond_out}\n{prefix}  ? {t}\n{prefix}  : {f}")
        }
        Expr::Object(fields) if !fields.is_empty() => {
            let prefix = " ".repeat(indent);
            let inner_prefix = " ".repeat(indent + 2);
            let mut lines = Vec::new();
            lines.push("{".to_string());
            for (key, val) in fields {
                // Insert any comments that precede this value's AST node
                let val_comments = take_comments(cmap, val.location.0);
                for c in &val_comments {
                    lines.push(format!("{inner_prefix}{c}"));
                }
                let chain = collect_pipe_chain(val);
                if chain.len() >= 3 {
                    let pipe_str = format_pipe_chain_commented(&chain, indent + 2, comments, val.location.0);
                    let pipe_str = pipe_str.trim_start();
                    lines.push(format!("{inner_prefix}{key}: {pipe_str},"));
                } else {
                    let val_str = emit_or_multiline_commented(val, indent + 4, cmap, comments);
                    lines.push(format!("{inner_prefix}{key}: {val_str},"));
                }
            }
            lines.push(format!("{prefix}}}"));
            lines.join("\n")
        }
        Expr::Array(items) if !items.is_empty() => {
            let prefix = " ".repeat(indent);
            let inner_prefix = " ".repeat(indent + 2);
            let mut lines = Vec::new();
            lines.push("[".to_string());
            for item in items {
                let item_comments = take_comments(cmap, item.location.0);
                for c in &item_comments {
                    lines.push(format!("{inner_prefix}{c}"));
                }
                let item_str = emit_or_multiline_commented(item, indent + 2, cmap, comments);
                lines.push(format!("{inner_prefix}{item_str},"));
            }
            lines.push(format!("{prefix}]"));
            lines.join("\n")
        }
        _ => {
            let node_comments = take_comments(cmap, expr.location.0);
            let prefix_str = format_comments(&node_comments, indent);
            let code = emit(expr, indent);
            format!("{prefix_str}{code}")
        }
    }
}

/// Find comments whose byte offset falls in `[range_start, range_end)`.
fn comments_in_range(comments: &[Comment], range_start: usize, range_end: usize) -> Vec<&str> {
    comments
        .iter()
        .filter(|c| c.offset >= range_start && c.offset < range_end)
        .map(|c| c.text.as_str())
        .collect()
}

/// Like `format_pipe_chain` but inserts comments between pipe steps.
///
/// For a chain `[Base, S1, S2, S3]`, each step Si has `subject_end` (end byte
/// of its subject in the source) and `expr_end` (end byte of the whole
/// expression up to and including Si).  Comments in the source range
/// `[Si.subject_end, Si.expr_end)` belong to step Si (they appear between
/// the previous code and the `| name(...)` of Si).
fn format_pipe_chain_commented(
    chain: &[ChainElement<'_>],
    indent: usize,
    comments: &[Comment],
    scope_start: usize,
) -> String {
    let prefix = " ".repeat(indent);
    let pipe_prefix = " ".repeat(indent + 2);
    let mut parts = Vec::with_capacity(chain.len());

    for (i, el) in chain.iter().enumerate() {
        match el {
            ChainElement::Base(expr) => {
                // Comments between scope_start and the base expression
                let base_comments = comments_in_range(comments, scope_start, expr.location.0);
                for c in &base_comments {
                    parts.push(format!("{prefix}{c}"));
                }
                parts.push(format!("{prefix}{}", emit(expr, indent)));
            }
            ChainElement::Transform {
                name,
                args,
                subject_end,
                expr_end,
            } => {
                // Comments in [subject_end, expr_end) belong before this step.
                let step_comments = comments_in_range(comments, *subject_end, *expr_end);
                for c in &step_comments {
                    parts.push(format!("{pipe_prefix}{c}"));
                }
                let args_str = format_args_list(args);
                if i == 0 {
                    parts.push(format!("{prefix}{name}{args_str}"));
                } else {
                    parts.push(format!("{pipe_prefix}| {name}{args_str}"));
                }
            }
            ChainElement::ExpressionTransform {
                name,
                expression,
                extra_args,
                subject_end,
                expr_end,
            } => {
                let step_comments = comments_in_range(comments, *subject_end, *expr_end);
                for c in &step_comments {
                    parts.push(format!("{pipe_prefix}{c}"));
                }
                let expr_str = emit(expression, 0);
                let extra = extra_args
                    .map(|n| format!(", {}", format_number(n)))
                    .unwrap_or_default();
                parts.push(format!("{pipe_prefix}| {name}({expr_str}{extra})"));
            }
            ChainElement::Reduce {
                init,
                expression,
                subject_end,
                expr_end,
            } => {
                let step_comments = comments_in_range(comments, *subject_end, *expr_end);
                for c in &step_comments {
                    parts.push(format!("{pipe_prefix}{c}"));
                }
                parts.push(format!(
                    "{pipe_prefix}| reduce({}, {})",
                    emit(init, 0),
                    emit(expression, 0)
                ));
            }
        }
    }

    parts.join("\n")
}

/// Collect all elements of a pipe chain (leftmost subject first).
fn collect_pipe_chain(expr: &Expression) -> Vec<ChainElement<'_>> {
    let mut elements = Vec::new();
    collect_pipe_chain_inner(expr, &mut elements);
    elements
}

enum ChainElement<'a> {
    Base(&'a Expression),
    Transform {
        name: &'a str,
        args: &'a Option<Vec<Box<Expression>>>,
        /// End byte of the subject expression in the source.
        subject_end: usize,
        /// End byte of the overall transform expression in the source.
        expr_end: usize,
    },
    ExpressionTransform {
        name: &'static str,
        expression: &'a Expression,
        extra_args: Option<f64>,
        subject_end: usize,
        expr_end: usize,
    },
    Reduce {
        init: &'a Expression,
        expression: &'a Expression,
        subject_end: usize,
        expr_end: usize,
    },
}

fn collect_pipe_chain_inner<'a>(expr: &'a Expression, out: &mut Vec<ChainElement<'a>>) {
    let expr_end = expr.location.1;
    match &expr.expression {
        Expr::Transform {
            name,
            subject,
            args,
        } => {
            collect_pipe_chain_inner(subject, out);
            out.push(ChainElement::Transform {
                name,
                args,
                subject_end: subject.location.1,
                expr_end,
            });
        }
        Expr::MapTransform {
            subject,
            name,
            args,
        }
        | Expr::SortByTransform {
            subject,
            name,
            args,
        }
        | Expr::AnyTransform {
            subject,
            name,
            args,
        }
        | Expr::AllTransform {
            subject,
            name,
            args,
        }
        | Expr::FindTransform {
            subject,
            name,
            args,
        }
        | Expr::FindIndexTransform {
            subject,
            name,
            args,
        }
        | Expr::FilterTransform {
            subject,
            name,
            args,
        } => {
            collect_pipe_chain_inner(subject, out);
            out.push(ChainElement::Transform {
                name,
                args,
                subject_end: subject.location.1,
                expr_end,
            });
        }
        Expr::ExpressionTransform {
            name,
            subject,
            expression,
            args,
        } => {
            collect_pipe_chain_inner(subject, out);
            let name_str = match name {
                ExpressionTransform::Map => "map",
                ExpressionTransform::Filter => "filter",
                ExpressionTransform::SortBy => "sortBy",
                ExpressionTransform::Any => "any",
                ExpressionTransform::All => "all",
                ExpressionTransform::Find => "find",
                ExpressionTransform::FindIndex => "findIndex",
                ExpressionTransform::Apply => "apply",
            };
            out.push(ChainElement::ExpressionTransform {
                name: name_str,
                expression,
                extra_args: *args,
                subject_end: subject.location.1,
                expr_end,
            });
        }
        Expr::ReduceExpression {
            subject,
            init,
            expression,
        } => {
            collect_pipe_chain_inner(subject, out);
            out.push(ChainElement::Reduce {
                init,
                expression,
                subject_end: subject.location.1,
                expr_end,
            });
        }
        _ => {
            out.push(ChainElement::Base(expr));
        }
    }
}

fn format_transform(subject: &str, name: &str, args: &Option<Vec<Box<Expression>>>) -> String {
    let args_str = format_args_list(args);
    format!("{subject} | {name}{args_str}")
}

fn format_args_list(args: &Option<Vec<Box<Expression>>>) -> String {
    match args {
        Some(args) if !args.is_empty() => {
            let parts: Vec<String> = args.iter().map(|a| emit(a, 0)).collect();
            format!("({})", parts.join(", "))
        }
        _ => String::new(),
    }
}

fn op_to_str(op: &OpCode) -> &'static str {
    match op {
        OpCode::Add => "+",
        OpCode::Subtract => "-",
        OpCode::Multiply => "*",
        OpCode::Divide => "/",
        OpCode::FloorDivide => "//",
        OpCode::Less => "<",
        OpCode::LessEqual => "<=",
        OpCode::Greater => ">",
        OpCode::GreaterEqual => ">=",
        OpCode::Equal => "==",
        OpCode::NotEqual => "!=",
        OpCode::And => "&&",
        OpCode::Or => "||",
        OpCode::Modulus => "%",
        OpCode::Exponent => "^",
        OpCode::In => "in",
        OpCode::Matches => "~",
        OpCode::Capture => "@",
        OpCode::CaptureMultiple => "@+",
    }
}

fn format_number(n: f64) -> String {
    if n == n.floor() && n.abs() < 1e15 {
        format!("{}", n as i64)
    } else {
        n.to_string()
    }
}

/// Return the binding precedence of a binary operator.
/// Lower values bind more loosely.
fn op_precedence(op: &OpCode) -> u8 {
    match op {
        OpCode::Or => 1,
        OpCode::And => 2,
        OpCode::In
        | OpCode::Equal
        | OpCode::NotEqual
        | OpCode::Less
        | OpCode::LessEqual
        | OpCode::Greater
        | OpCode::GreaterEqual
        | OpCode::Matches
        | OpCode::Capture
        | OpCode::CaptureMultiple => 3,
        OpCode::Add | OpCode::Subtract => 4,
        OpCode::Multiply | OpCode::Divide | OpCode::FloorDivide | OpCode::Modulus => 5,
        OpCode::Exponent => 6,
    }
}

/// Return the binding precedence of an expression.
/// Lower values bind more loosely (need parens more often as children).
fn expr_precedence(expr: &Expr) -> u8 {
    match expr {
        Expr::Conditional { .. } => 0,
        Expr::BinaryOperation { operation, .. } => op_precedence(operation),
        Expr::UnaryOperation { .. } => 7,
        Expr::Transform { .. }
        | Expr::MapTransform { .. }
        | Expr::SortByTransform { .. }
        | Expr::AnyTransform { .. }
        | Expr::AllTransform { .. }
        | Expr::FindTransform { .. }
        | Expr::FindIndexTransform { .. }
        | Expr::FilterTransform { .. }
        | Expr::ExpressionTransform { .. }
        | Expr::ReduceExpression { .. } => 8,
        _ => 10,
    }
}

/// Check whether an expression needs parentheses when used as the subject of
/// a dot or index operation.  Pipe expressions (transforms), conditionals,
/// binary operations, and unary operations all bind more loosely than
/// `.` / `[]`, so they must be wrapped.
fn needs_parens(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Transform { .. }
            | Expr::MapTransform { .. }
            | Expr::SortByTransform { .. }
            | Expr::AnyTransform { .. }
            | Expr::AllTransform { .. }
            | Expr::FindTransform { .. }
            | Expr::FindIndexTransform { .. }
            | Expr::FilterTransform { .. }
            | Expr::ExpressionTransform { .. }
            | Expr::ReduceExpression { .. }
            | Expr::Conditional { .. }
            | Expr::BinaryOperation { .. }
            | Expr::UnaryOperation { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_pipe_chain() {
        let result = format("a|b|c").unwrap();
        assert_eq!(result, "a\n  | b\n  | c");
    }

    #[test]
    fn test_format_two_pipe_stays_inline() {
        assert_eq!(format("a|b").unwrap(), "a | b");
    }

    #[test]
    fn test_format_short_ternary_stays_inline() {
        let result = format("x ? a : b").unwrap();
        assert_eq!(result, "x ? a : b");
    }

    #[test]
    fn test_format_long_pipe_chain_multiline() {
        let result = format("items | filter(this.active) | map(this.name) | join(\", \")").unwrap();
        assert!(
            result.contains('\n'),
            "Long pipe chain should be multiline, got: {result}"
        );
        assert!(result.contains("| filter"));
        assert!(result.contains("| map"));
        assert!(result.contains("| join"));
    }

    #[test]
    fn test_format_preserves_string_content() {
        let result = format("name | replace(\"hello world\", \"goodbye\")").unwrap();
        assert!(result.contains("hello world"));
        assert!(result.contains("goodbye"));
    }

    #[test]
    fn test_format_parse_error() {
        let result = format("foo +");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Parse error"));
    }

    #[test]
    fn test_format_binary_op() {
        assert_eq!(format("a+b").unwrap(), "a + b");
    }

    #[test]
    fn test_format_dot_access() {
        assert_eq!(format("customer.name").unwrap(), "customer.name");
    }

    #[test]
    fn test_format_array_literal() {
        assert_eq!(format("[1, 2, 3]").unwrap(), "[1, 2, 3]");
    }

    #[test]
    fn test_format_conditional_long() {
        let expr = "very_long_condition_variable == \"some_specific_value\" ? some_long_truthy_expression_result : some_long_falsy_expression_result";
        let result = format(expr).unwrap();
        assert!(
            result.contains('\n'),
            "Long ternary should be multiline, got: {result}"
        );
        assert!(result.contains("?"));
        assert!(result.contains(":"));
    }

    #[test]
    fn test_format_reduce() {
        // Grammar order is reduce(init, expression)
        let result = format("items | reduce(0, acc + this.price)").unwrap();
        assert_eq!(result, "items | reduce(0, acc + this.price)");
    }

    #[test]
    fn test_format_reduce_preserves_arg_order() {
        let result = format("items | reduce([], acc | push(this.name))").unwrap();
        assert!(
            result.contains("reduce([], acc | push(this.name))"),
            "reduce args should be (init, expression), got: {result}"
        );
    }

    #[test]
    fn test_format_filter_index() {
        let result = format("items[.active == true]").unwrap();
        assert!(result.contains("[.active == true]"));
    }

    #[test]
    fn test_format_unary_not() {
        assert_eq!(format("!active").unwrap(), "!active");
    }

    // --- Parenthesization tests ---

    #[test]
    fn test_format_dot_on_pipe_preserves_parens() {
        // (movies | first).genre must keep the parens
        let result = format("(movies | first).genre").unwrap();
        assert_eq!(result, "(movies | first).genre");
    }

    #[test]
    fn test_format_dot_on_pipe_chain_preserves_parens() {
        // The original bug: this expression was being mangled
        let result = format("(movies | first).genre | first | lowercase").unwrap();
        // Must contain parens around the inner pipe
        assert!(
            result.contains("(movies | first).genre"),
            "Must preserve parens around pipe subject of dot, got: {result}"
        );
    }

    #[test]
    fn test_format_index_on_pipe_preserves_parens() {
        let result = format("(items | sort)[0]").unwrap();
        assert_eq!(result, "(items | sort)[0]");
    }

    #[test]
    fn test_format_dot_on_simple_ident_no_parens() {
        // No unnecessary parens for simple dot access
        let result = format("customer.name").unwrap();
        assert_eq!(result, "customer.name");
    }

    #[test]
    fn test_format_roundtrip_dot_on_pipe() {
        // Format should produce a valid expression that re-parses identically
        let expr = "(movies | first).genre | uppercase";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(formatted, reparsed, "Formatting should be idempotent");
    }

    // --- Binary operation precedence tests ---

    #[test]
    fn test_format_binop_lower_prec_left() {
        // (a || b) + c — OR has lower prec than ADD, needs parens on left
        assert_eq!(format("(a || b) + c").unwrap(), "(a || b) + c");
    }

    #[test]
    fn test_format_binop_lower_prec_right() {
        // a + (b || c) — OR has lower prec than ADD, needs parens on right
        assert_eq!(format("a + (b || c)").unwrap(), "a + (b || c)");
    }

    #[test]
    fn test_format_binop_higher_prec_no_parens() {
        // a + b * c — MUL has higher prec than ADD, no parens needed
        assert_eq!(format("a + b * c").unwrap(), "a + b * c");
    }

    #[test]
    fn test_format_binop_higher_prec_left_needs_parens() {
        // (a + b) * c — ADD has lower prec than MUL, left needs parens
        assert_eq!(format("(a + b) * c").unwrap(), "(a + b) * c");
    }

    #[test]
    fn test_format_binop_equal_prec_right_needs_parens() {
        // a - (b + c) — same precedence level, right needs parens
        assert_eq!(format("a - (b + c)").unwrap(), "a - (b + c)");
    }

    #[test]
    fn test_format_binop_equal_prec_left_no_parens() {
        // a + b - c from AST (a + b) - c — left-assoc, left same prec is fine
        assert_eq!(format("a + b - c").unwrap(), "a + b - c");
    }

    #[test]
    fn test_format_conditional_in_binop() {
        // (a ? b : c) + d — conditional has lowest prec, needs parens
        assert_eq!(format("(a ? b : c) + d").unwrap(), "(a ? b : c) + d");
    }

    // --- Unary operation precedence tests ---

    #[test]
    fn test_format_unary_not_comparison() {
        // !(a == b) — comparison has lower prec than unary, needs parens
        assert_eq!(format("!(a == b)").unwrap(), "!(a == b)");
    }

    #[test]
    fn test_format_unary_not_matches() {
        // !(x ~ /pattern/) — match has lower prec, needs parens
        assert_eq!(
            format("!(x ~ /pattern/)").unwrap(),
            "!(x ~ /pattern/)"
        );
    }

    #[test]
    fn test_format_unary_not_binop() {
        // !(a + b) — arithmetic has lower prec, needs parens
        assert_eq!(format("!(a + b)").unwrap(), "!(a + b)");
    }

    #[test]
    fn test_format_unary_not_pipe_no_parens() {
        // !a | b — pipe has higher prec than unary, no parens needed
        // (parses as !(a | b) naturally)
        assert_eq!(format("!a | b").unwrap(), "!a | b");
    }

    #[test]
    fn test_format_unary_double_not() {
        assert_eq!(format("!!x").unwrap(), "!!x");
    }

    #[test]
    fn test_format_unary_neg_in_binop() {
        // -a + b — unary has higher prec than add, natural
        assert_eq!(format("-a + b").unwrap(), "-a + b");
    }

    #[test]
    fn test_format_unary_neg_of_binop() {
        // -(a + b) — needs parens
        assert_eq!(format("-(a + b)").unwrap(), "-(a + b)");
    }

    #[test]
    fn test_format_dot_on_unary_preserves_parens() {
        // (!a).b — unary result as dot subject needs parens
        assert_eq!(format("(!a).b").unwrap(), "(!a).b");
    }

    // --- Multiline object tests ---

    #[test]
    fn test_format_small_object_stays_flat() {
        assert_eq!(format("{a: 1, b: 2}").unwrap(), "{a: 1, b: 2}");
    }

    #[test]
    fn test_format_large_object_multiline() {
        let expr = "{key1: very_long_value_one, key2: very_long_value_two, key3: very_long_value_three, key4: value}";
        let result = format(expr).unwrap();
        assert!(
            result.contains('\n'),
            "Large object should be multiline, got: {result}"
        );
        assert!(result.starts_with('{'));
        assert!(result.ends_with('}'));
    }

    #[test]
    fn test_format_object_with_pipe_values() {
        let expr = "{name: items | sortByAttribute(\"name\") | first | apply(this.name), count: items | filter(this.active) | size}";
        let result = format(expr).unwrap();
        assert!(
            result.contains('\n'),
            "Object with pipe chains should be multiline, got: {result}"
        );
        assert!(result.contains("name:"));
        assert!(result.contains("| sort"));
    }

    // --- Roundtrip / idempotency tests ---

    #[test]
    fn test_format_idempotent_binop_parens() {
        let expr = "(a || b) + (c && d)";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(formatted, reparsed, "Formatting should be idempotent");
    }

    #[test]
    fn test_format_idempotent_unary_parens() {
        let expr = "!(x == y)";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(formatted, reparsed, "Formatting should be idempotent");
    }

    #[test]
    fn test_format_idempotent_complex_conditional() {
        let expr = "!(x ~ /[a-z]/) ? acc | push(this.name) : acc";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(
            formatted, reparsed,
            "Formatting should be idempotent, got: {formatted}"
        );
    }

    #[test]
    fn test_format_floor_div_with_pipe() {
        // (this | size) // 2 — pipe in left of floor-div
        // Since pipe has higher prec than //, this doesn't strictly need parens,
        // but we should verify the output is valid
        let expr = "(this | size) // 2";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(
            formatted, reparsed,
            "Floor-div with pipe should be idempotent, got: {formatted}"
        );
    }

    #[test]
    fn test_format_string_concat_with_pipe() {
        // '$' + this.revenue | toString — pipe binds tighter than +
        let expr = "\"$\" + this.revenue | toString";
        let formatted = format(expr).unwrap();
        let reparsed = format(&formatted).unwrap();
        assert_eq!(
            formatted, reparsed,
            "String concat with pipe should be idempotent, got: {formatted}"
        );
    }

    // --- Comment preservation tests ---

    #[test]
    fn test_format_comment_in_pipe_chain() {
        let expr = "items\n  # filter active\n  | filter(this.active)\n  | map(this.name)\n  | join(\", \")";
        let result = format(expr).unwrap();
        assert!(
            result.contains("# filter active"),
            "Comment should be preserved in pipe chain, got:\n{result}"
        );
    }

    #[test]
    fn test_format_comment_at_end() {
        let expr = "a | b | c\n# trailing comment";
        let result = format(expr).unwrap();
        assert!(
            result.contains("# trailing comment"),
            "Trailing comment should be preserved, got:\n{result}"
        );
    }

    #[test]
    fn test_format_comment_in_object() {
        let expr = "{\n  # this is the name\n  name: x | uppercase,\n  age: 42\n}";
        let result = format(expr).unwrap();
        assert!(
            result.contains("# this is the name"),
            "Comment in object should be preserved, got:\n{result}"
        );
    }

    #[test]
    fn test_format_multiple_comments() {
        let expr = "items\n  # step 1\n  | sort\n  # step 2\n  | first\n  | apply(this.name)";
        let result = format(expr).unwrap();
        assert!(
            result.contains("# step 1"),
            "First comment should be preserved, got:\n{result}"
        );
        assert!(
            result.contains("# step 2"),
            "Second comment should be preserved, got:\n{result}"
        );
    }

    #[test]
    fn test_format_no_comment_stays_same() {
        // Ensure no-comment expressions are unaffected
        let expr = "a + b";
        assert_eq!(format(expr).unwrap(), "a + b");
    }

    #[test]
    fn test_format_comment_not_in_string() {
        // '#' inside a string should NOT be treated as a comment
        let expr = "\"hello # world\"";
        let result = format(expr).unwrap();
        assert_eq!(result, "\"hello # world\"");
    }

    #[test]
    fn test_format_comment_forces_multiline() {
        // A short expression with a comment should still produce multiline
        let expr = "# comment\na + b";
        let result = format(expr).unwrap();
        assert!(
            result.contains("# comment"),
            "Comment should be preserved even for short expressions, got:\n{result}"
        );
    }

    #[test]
    fn test_format_complex_object_comments_not_duplicated() {
        // Regression: comments in one field's pipe chain must NOT leak into other fields.
        let expr = r#"{
  a: items
    | filter(this.x == 1)
    # only first half
    | apply(this | range(0, this | size // 2))
    | pick('name'),
  # sort descending
  b: items
    | sortByAttribute('score', -1)
    | first,
  c: items
    | map(this.name)
    | size,
}"#;
        let result = format(expr).unwrap();

        // "# only first half" should appear exactly once, inside field `a`
        assert_eq!(
            result.matches("# only first half").count(),
            1,
            "Comment '# only first half' should appear exactly once, got:\n{result}"
        );
        // "# sort descending" should appear exactly once, before field `b`
        assert_eq!(
            result.matches("# sort descending").count(),
            1,
            "Comment '# sort descending' should appear exactly once, got:\n{result}"
        );
        // Field `c` should NOT contain either comment
        let c_field_pos = result.find("c: items").expect("must contain c: items");
        let c_tail = &result[c_field_pos..];
        assert!(
            !c_tail.contains("# only first half") && !c_tail.contains("# sort descending"),
            "Field c should not contain any comments from other fields, got:\n{result}"
        );
    }

    #[test]
    fn test_extract_comments_basic() {
        let comments = extract_comments("a + b # inline\n# full line\nc");
        assert_eq!(comments.len(), 2);
        assert_eq!(comments[0].text, "# inline");
        assert_eq!(comments[1].text, "# full line");
    }

    #[test]
    fn test_extract_comments_skips_strings() {
        let comments = extract_comments("\"no # comment\" + x");
        assert!(
            comments.is_empty(),
            "Should not find comment inside string, got: {:?}",
            comments
        );
    }
}
