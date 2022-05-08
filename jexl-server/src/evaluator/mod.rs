use anyhow::Context;
use axum::{
    extract::Extension, http::StatusCode, response::IntoResponse, routing::post, Json, Router,
};
use jexl_3000::Evaluator;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use tracing::trace_span;
use tracing_futures::{Instrument, WithSubscriber};
use utoipa::Component;

use crate::error::Result;

#[derive(Component, Debug, Deserialize)]
#[component(example = json!({"context": {"users": [{"name": "Bob", "age": 32}, {"name": "Alice", "age": 72}]}, "expressions": ["users | pick('name')", "users | map(this.age) | mean"]}))]
pub struct Input {
    context: Value,
    expressions: Vec<String>,
}

#[derive(Serialize)]
struct EvaluationResult {
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

#[derive(Serialize)]

struct Response {
    evaluated: Vec<EvaluationResult>,
}

pub fn router() -> Router {
    Router::new().nest(
        "/evaluate",
        Router::new()
            .route("/tokio", post(evaluate_tokio))
            .route("/par", post(evaluate_par))
            .route("/", post(evaluate)),
    )
}

#[utoipa::path(
    get,
    path = "/simple",
    request_body = Input,
    responses(
        (status = 200, description = "Evaluated expression", body = Value),
        (status = 500, description = "An error occured during the evaluation")
    ),
)]
async fn evaluate(
    Extension(ref evaluator): Extension<Arc<Evaluator<'static>>>,
    Json(req): Json<Input>,
) -> Result<impl IntoResponse> {
    let evaluated = req
        .expressions
        .iter()
        .map(
            |expression| match evaluator.eval_in_context(expression, &req.context) {
                Ok(value) => EvaluationResult {
                    result: Some(value),
                    error: None,
                },
                Err(err) => EvaluationResult {
                    result: None,
                    error: Some(err.to_string()),
                },
            },
        )
        .collect::<Vec<_>>();
    Ok((StatusCode::OK, Json(Response { evaluated })))
}

async fn evaluate_par(
    Extension(ref evaluator): Extension<Arc<Evaluator<'static>>>,
    Json(req): Json<Input>,
) -> Result<impl IntoResponse> {
    let evaluated = req
        .expressions
        .par_iter()
        .map(move |expression| {
            let span = trace_span!("processing expression", expression = &expression.as_str());
            span.in_scope(
                || match evaluator.eval_in_context(expression, &req.context) {
                    Ok(value) => EvaluationResult {
                        result: Some(value),
                        error: None,
                    },
                    Err(err) => EvaluationResult {
                        result: None,
                        error: Some(err.to_string()),
                    },
                },
            )
        })
        .collect::<Vec<_>>();
    Ok((StatusCode::OK, Json(Response { evaluated })))
}

async fn evaluate_tokio(
    Extension(ref evaluator): Extension<Arc<Evaluator<'static>>>,
    Json(req): Json<Input>,
) -> Result<impl IntoResponse> {
    // managing async + multithread from https://www.fpcomplete.com/blog/captures-closures-async/
    let evaluated = req
        .expressions
        .into_iter()
        .map(move |expression| {
            let evaluator = evaluator.clone();
            let context = req.context.clone();
            let expression_clone = expression.clone();
            tokio::task::spawn(async move {
                match evaluator.eval_in_context(&expression_clone, &context) {
                    Ok(value) => EvaluationResult {
                        result: Some(value),
                        error: None,
                    },
                    Err(err) => EvaluationResult {
                        result: None,
                        error: Some(err.to_string()),
                    },
                }
            })
            .instrument(trace_span!(
                "evaluating expression",
                expression = expression.as_str()
            ))
            .with_current_subscriber()
        })
        .collect::<Vec<_>>();
    let evaluated = futures::future::join_all(evaluated)
        .await
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .context("failed to unwrap")?;
    Ok((StatusCode::OK, Json(Response { evaluated })))
}

#[cfg(test)]
mod tests {
    use crate::http::tests::build_app;

    use axum::{body::Body, http::Request};
    use hyper::{Method, StatusCode};
    use serde_json::{json, Value};
    use tower::ServiceExt;

    #[tokio::test]
    async fn e2e_evaluate() {
        let app = build_app().await;
        let response = app
            .clone()
            .oneshot(
                Request::builder()
                    .method(Method::POST)
                    .header("Content-type", "application/json")
                    .uri("/evaluate")
                    .body(Body::from(
                        serde_json::to_string(&json!({
                            "context": {
                                "a": 1,
                                "b": 2
                            },
                            "expressions": ["a + b", "a * b", "lol + 1"]
                        }))
                        .unwrap(),
                    ))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let body: Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(
            body["evaluated"],
            json!([{"result": 3f64},{"result": 2f64}, {"error": "Identifier 'lol' is undefined"}])
        );
    }
}
