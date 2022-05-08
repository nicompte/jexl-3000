use anyhow::Context;
use axum::{
    extract::Extension, http::header, http::Method, middleware, response::IntoResponse,
    routing::get, Router,
};
use evaluator::Input;
use std::{net::SocketAddr, sync::Arc};
use tower::ServiceBuilder;
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use utoipa::OpenApi;

use crate::{app_config::AppConfig, evaluator};

mod http_metrics;

mod up;

pub use crate::error::Error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

async fn app() -> Result<Router> {
    let evaluator = jexl_3000::build_evaluator();
    let shared_evaluator = Arc::new(evaluator);
    let recorder_handle = http_metrics::setup_metrics_recorder();

    let router = evaluator::router()
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(vec![Method::POST, Method::GET]),
        )
        .layer(
            ServiceBuilder::new()
                .layer(Extension(shared_evaluator))
                .layer(TraceLayer::new_for_http()),
        )
        .route_layer(middleware::from_fn(http_metrics::track_metrics))
        .route(
            "/metrics",
            get(|| http_metrics::render_metrics(recorder_handle)),
        )
        .merge(up::router())
        .route("/openapi", get(open_api));
    Ok(router)
}

pub async fn serve(config: AppConfig) -> anyhow::Result<()> {
    let port = config.port;
    let app = app().await?;

    axum::Server::bind(&SocketAddr::from(([0, 0, 0, 0], port)))
        .serve(app.into_make_service())
        .await
        .context("error running HTTP server")
}
#[derive(OpenApi)]
#[openapi(handlers(evaluator::evaluate), components(Input))]
struct ApiDoc;

async fn open_api() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "application/json")],
        ApiDoc::openapi().to_json().unwrap(),
    )
}

#[cfg(test)]
pub mod tests {

    use super::*;

    pub async fn build_app() -> Router {
        dotenv::from_filename(".env.tests").ok();
        app().await.unwrap()
    }
}
