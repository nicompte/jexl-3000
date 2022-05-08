use axum::{http::StatusCode, response::IntoResponse, routing::get, Json, Router};
use serde::Serialize;

use crate::http::Result;

#[derive(Serialize)]
struct EmptyStruct {}

#[derive(Serialize)]
struct UpOutput {
    status: String,
    info: EmptyStruct,
    error: EmptyStruct,
    details: EmptyStruct,
}

pub fn router() -> Router {
    Router::new().nest("/up", Router::new().route("/", get(up)))
}

async fn up() -> Result<impl IntoResponse> {
    Ok((
        StatusCode::OK,
        Json(UpOutput {
            status: "ok".to_string(),
            info: EmptyStruct {},
            error: EmptyStruct {},
            details: EmptyStruct {},
        }),
    ))
}

#[cfg(test)]
mod tests {
    use crate::http::tests::build_app;

    use super::*;
    use axum::{body::Body, http::Request};
    use serde_json::{json, Value};
    use tower::ServiceExt;

    #[tokio::test]
    async fn e2e_test_up() {
        let app = build_app().await;
        let response = app
            .oneshot(Request::builder().uri("/up").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let body: Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(
            body,
            json!({
                "status": "ok",
                "info": {},
                "error": {},
                "details": {}
            })
        );
    }
}
