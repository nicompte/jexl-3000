use super::registry::TRANSFORMER_REGISTRY;
use super::types::{ParameterInfo, SchemaIndex, SignatureHelp};

/// Provide signature help for the transform at the cursor position.
///
/// Returns `Some(SignatureHelp)` when the cursor is inside a transform's
/// argument list (between the parentheses), `None` otherwise.
#[must_use]
pub fn signature_help(
    expr: &str,
    offset: usize,
    _schema_index: Option<&SchemaIndex>,
) -> Option<SignatureHelp> {
    let prefix = &expr[..offset.min(expr.len())];

    // Walk backwards through the prefix tracking paren depth to find the
    // opening `(` whose transform name we want.  We also count commas at
    // depth 0 (relative to our target paren) to determine `active_parameter`.
    let mut paren_depth: i32 = 0;
    let mut commas_at_depth_zero = 0usize;

    for (i, c) in prefix.char_indices().rev() {
        match c {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    // Found the unmatched opening paren — extract the
                    // transform name immediately before it.
                    let before_paren = prefix[..i].trim_end();
                    let name = extract_trailing_ident(before_paren);
                    if name.is_empty() {
                        return None;
                    }

                    let descriptor = TRANSFORMER_REGISTRY.get(name)?;

                    let parameters: Vec<ParameterInfo> = descriptor
                        .args
                        .iter()
                        .map(|a| {
                            let type_str = a
                                .types
                                .iter()
                                .map(std::string::ToString::to_string)
                                .collect::<Vec<_>>()
                                .join(" | ");
                            let label = if a.required {
                                format!("{}: {type_str}", a.name)
                            } else {
                                format!("{}?: {type_str}", a.name)
                            };
                            ParameterInfo {
                                label,
                                documentation: if a.description.is_empty() {
                                    None
                                } else {
                                    Some(a.description.to_string())
                                },
                            }
                        })
                        .collect();

                    let param_labels: Vec<&str> =
                        parameters.iter().map(|p| p.label.as_str()).collect();
                    let label = format!("{}({})", name, param_labels.join(", "));

                    return Some(SignatureHelp {
                        label,
                        documentation: if descriptor.description.is_empty() {
                            None
                        } else {
                            Some(descriptor.description.to_string())
                        },
                        parameters,
                        active_parameter: commas_at_depth_zero,
                    });
                }
            }
            ',' if paren_depth == 0 => {
                commas_at_depth_zero += 1;
            }
            _ => {}
        }
    }

    None
}

/// Extract the trailing identifier from a string slice.
fn extract_trailing_ident(s: &str) -> &str {
    let end = s.len();
    let start = s
        .char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphanumeric() || *c == '_')
        .last()
        .map_or(end, |(i, _)| i);
    &s[start..end]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sig(expr: &str, offset: usize) -> Option<SignatureHelp> {
        signature_help(expr, offset, None)
    }

    #[test]
    fn test_signature_inside_replace_first_arg() {
        let result = sig("name | replace(", 15).unwrap();
        assert_eq!(result.active_parameter, 0);
        assert!(result.label.starts_with("replace("));
        assert_eq!(result.parameters.len(), 2);
    }

    #[test]
    fn test_signature_inside_replace_second_arg() {
        let result = sig("name | replace(\"a\", ", 20).unwrap();
        assert_eq!(result.active_parameter, 1);
    }

    #[test]
    fn test_signature_outside_parens() {
        assert!(sig("name | uppercase", 16).is_none());
    }

    #[test]
    fn test_signature_join_optional_arg() {
        let result = sig("items | join(", 13).unwrap();
        assert_eq!(result.active_parameter, 0);
        assert!(result.label.contains("join("));
        // join has an optional separator argument
        assert!(result.parameters[0].label.contains('?'));
    }

    #[test]
    fn test_signature_unknown_transform() {
        assert!(sig("name | unknownTransform(", 24).is_none());
    }

    #[test]
    fn test_signature_nested_parens() {
        // Cursor is inside the outer replace, not inside a nested call
        let result = sig("name | replace(uppercase(", 25);
        // uppercase is a transform but has no args in registry (0 args) —
        // so if it's found, it should show uppercase's signature
        // Actually uppercase has 0 args, so it will match with 0 params
        assert!(result.is_some());
    }

    #[test]
    fn test_active_parameter_with_spaces() {
        let result = sig("name | replace( \"a\" , ", 21).unwrap();
        assert_eq!(result.active_parameter, 1);
    }

    #[test]
    fn test_signature_at_opening_paren() {
        let result = sig("name | split(", 13).unwrap();
        assert_eq!(result.active_parameter, 0);
        assert!(result.label.starts_with("split("));
    }
}
