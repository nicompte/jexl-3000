use std::error::Error;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Evaluator(jexl_3000::Evaluator<'static>);

#[wasm_bindgen]
impl Evaluator {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Evaluator {
        console_error_panic_hook::set_once();
        Evaluator(jexl_3000::build_evaluator())
    }

    pub fn evaluate(&mut self, input: &str, context: JsValue) -> Result<JsValue, JsValue> {
        let context = if context.is_undefined() || context.is_null() {
            serde_json::json!({})
        } else {
            serde_wasm_bindgen::from_value(context).map_err(|e| JsValue::from(e.to_string()))?
        };

        let evaluation = self.0.eval_in_context(input, &context).map_err(|error| {
            let cause = if let Some(source) = error.source() {
                anyhow::Chain::new(source)
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
            } else {
                vec![error.to_string()]
            };

            let err = serde_json::json!({
                "error": "EvaluationError",
                "caused_by": cause,
            });

            serde_wasm_bindgen::to_value(&err).unwrap()
        })?;

        serde_wasm_bindgen::to_value(&evaluation).map_err(|e| JsValue::from(e.to_string()))
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
