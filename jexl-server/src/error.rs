use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use tracing::error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Internal server error")]
    Anyhow(#[from] anyhow::Error),
}

impl Error {
    fn status_code(&self) -> StatusCode {
        match self {
            Self::Anyhow(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        let message = match &self {
            Self::Anyhow(ref e) => {
                error!("Generic error: {:?}", e);
                e.root_cause().to_string()
            }
        };

        let body = serde_json::json!({
            "error": self.to_string(),
            "message": message
        });

        (self.status_code(), Json(body)).into_response()
    }
}
