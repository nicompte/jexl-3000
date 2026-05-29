use js_sys::{Array, Object, Reflect};
use serde::Serialize;
use std::error::Error;
use std::sync::LazyLock;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::*;

static EMPTY_CONTEXT: LazyLock<serde_json::Value> = LazyLock::new(|| serde_json::json!({}));

static SERIALIZER: LazyLock<serde_wasm_bindgen::Serializer> =
    LazyLock::new(|| serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true));

#[wasm_bindgen]
pub struct Evaluator(jexl_3000::Evaluator<'static>);

#[wasm_bindgen]
impl Evaluator {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Evaluator {
        Evaluator(jexl_3000::build_evaluator())
    }

    pub fn evaluate(&self, input: &str, context: JsValue) -> Result<JsValue, JsValue> {
        let context_opt: Option<serde_json::Value> = if context.is_undefined() || context.is_null()
        {
            None
        } else {
            Some(
                serde_wasm_bindgen::from_value(context)
                    .map_err(|e| JsValue::from(e.to_string()))?,
            )
        };
        let context = context_opt.as_ref().unwrap_or(&EMPTY_CONTEXT);

        let evaluation = self.0.eval_in_context(input, context).map_err(|error| {
            let err_obj = Object::new();
            let _ = Reflect::set(
                &err_obj,
                &JsValue::from_str("error"),
                &JsValue::from_str("EvaluationError"),
            );
            let cause_arr = Array::new();
            let mut current: Option<&dyn Error> = error.source();
            if current.is_none() {
                cause_arr.push(&JsValue::from_str(&error.to_string()));
            } else {
                while let Some(e) = current {
                    cause_arr.push(&JsValue::from_str(&e.to_string()));
                    current = e.source();
                }
            }
            let _ = Reflect::set(&err_obj, &JsValue::from_str("caused_by"), &cause_arr);
            JsValue::from(err_obj)
        })?;

        evaluation
            .serialize(&*SERIALIZER)
            .map_err(|e| JsValue::from(e.to_string()))
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// LanguageService (behind feature flag)
// ---------------------------------------------------------------------------

#[cfg(feature = "language-service")]
#[wasm_bindgen]
pub struct LanguageService(jexl_3000::language_service::LanguageService);

#[cfg(feature = "language-service")]
#[wasm_bindgen]
impl LanguageService {
    #[wasm_bindgen(constructor)]
    pub fn new() -> LanguageService {
        LanguageService(jexl_3000::language_service::LanguageService::new())
    }

    #[wasm_bindgen(js_name = setSchema)]
    pub fn set_schema(&mut self, schema: JsValue) -> Result<(), JsValue> {
        let schema: serde_json::Value =
            serde_wasm_bindgen::from_value(schema).map_err(|e| JsValue::from(e.to_string()))?;
        self.0.set_schema(schema);
        Ok(())
    }

    #[wasm_bindgen(js_name = setContext)]
    pub fn set_context(&mut self, context: JsValue) -> Result<(), JsValue> {
        let context: serde_json::Value =
            serde_wasm_bindgen::from_value(context).map_err(|e| JsValue::from(e.to_string()))?;
        self.0.set_context(context);
        Ok(())
    }

    #[wasm_bindgen(js_name = clearSchema)]
    pub fn clear_schema(&mut self) {
        self.0.clear_schema();
    }

    #[wasm_bindgen(js_name = clearContext)]
    pub fn clear_context(&mut self) {
        self.0.clear_context();
    }

    pub fn validate(&mut self, expr: &str) -> JsValue {
        let diags = self.0.validate(expr);
        diags.serialize(&*SERIALIZER).unwrap_or(JsValue::NULL)
    }

    pub fn completions(&self, expr: &str, offset: usize) -> JsValue {
        let items = self.0.completions(expr, offset);
        items.serialize(&*SERIALIZER).unwrap_or(JsValue::NULL)
    }

    pub fn completions_with_diagnostics(&mut self, expr: &str, offset: usize) -> JsValue {
        let result = self.0.completions_with_diagnostics(expr, offset);
        result.serialize(&*SERIALIZER).unwrap_or(JsValue::NULL)
    }

    pub fn hover(&mut self, expr: &str, offset: usize) -> JsValue {
        let info = self.0.hover(expr, offset);
        info.serialize(&*SERIALIZER).unwrap_or(JsValue::NULL)
    }

    #[wasm_bindgen(js_name = signatureHelp)]
    pub fn signature_help(&self, expr: &str, offset: usize) -> JsValue {
        let info = self.0.signature_help(expr, offset);
        info.serialize(&*SERIALIZER).unwrap_or(JsValue::NULL)
    }

    pub fn format(&self, expr: &str) -> JsValue {
        match self.0.format(expr) {
            Ok(formatted) => JsValue::from_str(&formatted),
            Err(err) => {
                let err_obj = Object::new();
                let _ = Reflect::set(
                    &err_obj,
                    &JsValue::from_str("error"),
                    &JsValue::from_str(&err),
                );
                JsValue::from(err_obj)
            }
        }
    }
}

#[cfg(feature = "language-service")]
impl Default for LanguageService {
    fn default() -> Self {
        Self::new()
    }
}
