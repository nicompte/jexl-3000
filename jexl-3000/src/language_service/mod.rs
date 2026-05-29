//! Language service for jexl-3000 expressions.
//!
//! Provides schema-aware validation, completion, and hover information
//! for JEXL expressions. JSON Schema is used as the primary source of
//! type information, with an optional runtime context as fallback.
//!
//! # Example
//!
//! ```rust
//! use jexl_3000::language_service::LanguageService;
//! use serde_json::json;
//!
//! let schema = json!({
//!     "type": "object",
//!     "properties": {
//!         "name": { "type": "string" },
//!         "age": { "type": "number" }
//!     }
//! });
//!
//! let mut svc = LanguageService::new();
//! svc.set_schema(schema);
//!
//! // Validate
//! let diags = svc.validate("name | uppercase");
//! assert!(diags.is_empty());
//!
//! // Completions
//! let items = svc.completions("name | ", 7);
//! assert!(items.iter().any(|i| i.label == "uppercase"));
//!
//! // Hover
//! let info = svc.hover("name", 2);
//! assert!(info.is_some());
//! ```

pub mod ast_walk;
pub mod completion;
pub mod format;
pub mod hover;
pub mod registry;
pub mod schema;
pub mod signature;
pub mod types;
pub mod validation;

pub use types::{
    CompletionItem, CompletionKind, CompletionResult, Diagnostic, DiagnosticSeverity, HoverInfo,
    PrimitiveType, SchemaEntry, SchemaIndex, SignatureHelp,
};

use jexl_parser::ast::Expression;
use jexl_parser::Parser;
use schema::build_schema_index;
use schema::update_schema_subtree;
use serde_json::Value;
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};

/// The main language service struct.
///
/// Holds an optional JSON Schema (with a pre-built index for fast lookups),
/// an optional runtime context for fallback property discovery, and a small
/// parse cache so repeated calls on the same expression avoid re-parsing.
pub struct LanguageService {
    schema: Option<Value>,
    schema_index: Option<SchemaIndex>,
    context: Option<Value>,
    /// Small LRU parse cache: expression hash → parsed AST.
    parse_cache: HashMap<u64, Expression>,
    /// Maximum number of cached ASTs.
    parse_cache_cap: usize,
}

fn hash_expr(expr: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    expr.hash(&mut hasher);
    hasher.finish()
}

impl LanguageService {
    /// Create a new language service with no schema or context.
    #[must_use] 
    pub fn new() -> Self {
        Self {
            schema: None,
            schema_index: None,
            context: None,
            parse_cache: HashMap::new(),
            parse_cache_cap: 16,
        }
    }

    /// Try to get a cached AST, or parse and cache it.
    /// Returns `None` if the expression can't be parsed.
    fn cached_parse(&mut self, expr: &str) -> Option<Expression> {
        let key = hash_expr(expr);
        if let Some(ast) = self.parse_cache.get(&key) {
            return Some(ast.clone());
        }
        let ast = Parser::parse(expr).ok()?;
        if self.parse_cache.len() >= self.parse_cache_cap {
            // Evict all — simple reset when cap is reached
            self.parse_cache.clear();
        }
        self.parse_cache.insert(key, ast.clone());
        Some(ast)
    }

    /// Set the JSON Schema used for type info, validation, and completions.
    /// This rebuilds the internal schema index.
    pub fn set_schema(&mut self, schema: Value) {
        self.schema_index = Some(build_schema_index(&schema));
        self.schema = Some(schema);
    }

    /// Set the runtime context object used as fallback for property discovery.
    pub fn set_context(&mut self, context: Value) {
        self.context = Some(context);
    }

    /// Clear the schema and its index.
    pub fn clear_schema(&mut self) {
        self.schema = None;
        self.schema_index = None;
    }

    /// Clear the runtime context.
    pub fn clear_context(&mut self) {
        self.context = None;
    }

    /// Update a subtree of the schema at the given dot-separated path without
    /// rebuilding the entire index. Useful when only part of the schema changes.
    ///
    /// Does nothing if no schema has been set.
    #[allow(clippy::needless_pass_by_value)]
    pub fn update_schema_subtree(&mut self, path: &str, new_schema: Value) {
        if let Some(index) = &mut self.schema_index {
            update_schema_subtree(index, path, &new_schema);
        }
    }

    /// Validate an expression and return diagnostics.
    ///
    /// Uses the parse cache to avoid re-parsing when possible.
    pub fn validate(&mut self, expr: &str) -> Vec<Diagnostic> {
        if let Some(ast) = self.cached_parse(expr) {
            validation::validate_ast(&ast, self.schema.as_ref(), self.context.as_ref())
        } else {
            // Parse failed — delegate to validate() which produces the parse error diagnostic
            validation::validate(expr, self.schema.as_ref(), self.context.as_ref())
        }
    }

    /// Generate completion items for the expression at the given byte offset.
    #[must_use] 
    pub fn completions(&self, expr: &str, offset: usize) -> Vec<CompletionItem> {
        completion::completions(expr, offset, self.schema_index.as_ref(), self.context.as_ref())
    }

    /// Get hover information for the element at the given byte offset.
    ///
    /// Uses the parse cache to avoid re-parsing when possible.
    pub fn hover(&mut self, expr: &str, offset: usize) -> Option<HoverInfo> {
        if let Some(ast) = self.cached_parse(expr) {
            hover::hover_ast(&ast, offset, self.schema_index.as_ref())
        } else {
            None
        }
    }

    /// Get signature help for a transform at the given byte offset.
    ///
    /// Returns help when the cursor is inside a transform's argument list.
    #[must_use]
    pub fn signature_help(&self, expr: &str, offset: usize) -> Option<SignatureHelp> {
        signature::signature_help(expr, offset, self.schema_index.as_ref())
    }

    /// Format / pretty-print a JEXL expression.
    ///
    /// Returns `Ok(formatted)` on success, or `Err(message)` on parse failure.
    pub fn format(&self, expr: &str) -> Result<String, String> {
        format::format(expr)
    }

    /// Generate completion items together with inline diagnostics for the expression.
    ///
    /// This combines `completions()` and `validate()` in a single call,
    /// useful for editors that want to show warnings alongside suggestions.
    pub fn completions_with_diagnostics(
        &mut self,
        expr: &str,
        offset: usize,
    ) -> types::CompletionResult {
        let items =
            completion::completions(expr, offset, self.schema_index.as_ref(), self.context.as_ref());
        let diagnostics = if let Some(ast) = self.cached_parse(expr) {
            validation::validate_ast(&ast, self.schema.as_ref(), self.context.as_ref())
        } else {
            validation::validate(expr, self.schema.as_ref(), self.context.as_ref())
        };
        types::CompletionResult { items, diagnostics }
    }
}

impl Default for LanguageService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_full_workflow() {
        let mut svc = LanguageService::new();
        svc.set_schema(json!({
            "type": "object",
            "properties": {
                "name": { "type": "string", "description": "Person's name" },
                "age": { "type": "number" },
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "label": { "type": "string" }
                        }
                    }
                }
            },
            "required": ["name"]
        }));

        // Validate valid expression
        let diags = svc.validate("name | uppercase");
        assert!(diags.is_empty(), "Expected no diagnostics, got: {diags:?}");

        // Validate invalid expression
        let diags = svc.validate("foo +");
        assert!(!diags.is_empty());
        assert!(diags[0].severity == DiagnosticSeverity::Error);

        // Validate undefined property
        let diags = svc.validate("unknown_var");
        assert!(diags.iter().any(|d| d.message.contains("not defined")));

        // Completions after dot
        let items = svc.completions("name.", 5);
        // `name` is a string, no sub-properties — should be empty
        assert!(items.is_empty());

        // Completions for identifiers
        let items = svc.completions("n", 1);
        assert!(items.iter().any(|i| i.label == "name"));

        // Completions after pipe
        let items = svc.completions("name | ", 7);
        assert!(items.iter().any(|i| i.label == "uppercase"));

        // Hover on identifier
        let info = svc.hover("name", 2);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("string"));

        // Hover on transform
        let info = svc.hover("name | uppercase", 10);
        assert!(info.is_some());
    }

    #[test]
    fn test_context_fallback() {
        let mut svc = LanguageService::new();
        svc.set_context(json!({
            "user": {
                "name": "Alice",
                "email": "alice@example.com"
            }
        }));

        let items = svc.completions("user.", 5);
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(items.iter().any(|i| i.label == "email"));
    }

    #[test]
    fn test_array_item_completions() {
        let mut svc = LanguageService::new();
        svc.set_schema(json!({
            "type": "object",
            "properties": {
                "orders": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": { "type": "number" },
                            "product": { "type": "string" }
                        }
                    }
                }
            }
        }));

        // `this.` inside map lambda should show array item properties
        let items = svc.completions("orders | map(this.", 18);
        assert!(
            items.iter().any(|i| i.label == "id"),
            "Expected 'id' in: {items:?}"
        );
    }

    // --- Phase 3: completions_with_diagnostics ---

    #[test]
    fn test_completions_with_diagnostics() {
        let mut svc = LanguageService::new();
        svc.set_schema(json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" },
                "age": { "type": "number" }
            }
        }));

        // Valid expression — should have completions and no diagnostics
        let result = svc.completions_with_diagnostics("name", 4);
        assert!(!result.items.is_empty(), "Should have completion items");
        assert!(result.diagnostics.is_empty(), "Should have no diagnostics for valid expr, got: {:?}", result.diagnostics);

        // Expression with unknown property — diagnostics should report it
        let result = svc.completions_with_diagnostics("unknown_var", 11);
        assert!(!result.diagnostics.is_empty(), "Should have warning about unknown_var");
    }

    // --- Phase 4: Incremental schema update ---

    #[test]
    fn test_update_schema_subtree() {
        let mut svc = LanguageService::new();
        svc.set_schema(json!({
            "type": "object",
            "properties": {
                "user": {
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" }
                    }
                }
            }
        }));

        // Initially, user has only 'name'
        let items = svc.completions("user.", 5);
        assert!(items.iter().any(|i| i.label == "name"));
        assert!(!items.iter().any(|i| i.label == "email"));

        // Update user subtree to add 'email'
        svc.update_schema_subtree("user", json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" },
                "email": { "type": "string" }
            }
        }));

        let items = svc.completions("user.", 5);
        assert!(items.iter().any(|i| i.label == "name"), "Should still have name");
        assert!(items.iter().any(|i| i.label == "email"), "Should now have email");
    }

    // --- Phase 4: Parse cache ---

    #[test]
    fn test_parse_cache_consistency() {
        let mut svc = LanguageService::new();
        svc.set_schema(json!({
            "type": "object",
            "properties": {
                "name": { "type": "string" }
            }
        }));

        // First call populates the cache
        let hover1 = svc.hover("name", 2);
        assert!(hover1.is_some());

        // Second call should return same result (from cache)
        let hover2 = svc.hover("name", 2);
        assert_eq!(hover1, hover2);

        // Validate also uses the cache — should not re-parse
        let diags = svc.validate("name");
        assert!(diags.is_empty());
    }
}
