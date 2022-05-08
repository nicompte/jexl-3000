#![deny(clippy::all)]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

use napi::bindgen_prelude::*;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde_json::value::Value;

#[macro_use]
extern crate napi_derive;

#[napi]
pub struct Evaluator(jexl_3000::Evaluator<'static>);

#[napi]
impl Evaluator {
  #[napi(constructor)]
  pub fn new() -> Self {
    Evaluator(jexl_3000::build_evaluator())
  }

  #[napi]
  pub fn evaluate(&self, expression: String, context: Option<Value>) -> Result<Value> {
    let context = context.unwrap_or_else(|| serde_json::json!({}));
    self
      .0
      .eval_in_context(&expression, &context)
      .map_err(|e| Error::new(Status::GenericFailure, e.to_string()))
  }

  #[napi]
  #[allow(non_snake_case)]
  pub fn evaluateMultiple(
    &self,
    expressions: Vec<String>,
    context: Option<Value>,
  ) -> Vec<Result<Value>> {
    let context = context.unwrap_or_else(|| serde_json::json!({}));
    expressions
      .par_iter()
      .map(|expression| {
        self
          .0
          .eval_in_context(expression, &context)
          .map_err(|e| Error::new(Status::InvalidArg, e.to_string()))
      })
      .collect::<Vec<_>>()
  }
}

impl Default for Evaluator {
  fn default() -> Self {
    Self::new()
  }
}
