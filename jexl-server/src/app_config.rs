use std::env;

use serde::{self, Deserialize};

macro_rules! from_env {
    ($input:expr) => {
        env::var($input).expect(&format!("missing {} environment variable", $input))
    };
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum LogFormat {
    Full,
    Pretty,
    Json,
}

impl TryFrom<String> for LogFormat {
    type Error = &'static str;

    fn try_from(log_format: String) -> Result<Self, Self::Error> {
        match log_format.as_str() {
            "full" => Ok(Self::Full),
            "pretty" => Ok(Self::Pretty),
            "json" => Ok(Self::Json),
            _ => Err("invalid log format"),
        }
    }
}

pub struct Telemetry {
    pub opentelemetry: bool,
    pub log_format: LogFormat,
    pub opentelemetry_endpoint: Option<String>,
}

impl Telemetry {
    pub fn build() -> Self {
        Telemetry {
            opentelemetry: true,
            log_format: env::var("LOG_FORMAT")
                .map(|log_format| {
                    log_format
                        .try_into()
                        .expect(r#"invalid LOG_FORMAT, should be "pretty", "json" or "full""#)
                })
                .unwrap_or(LogFormat::Pretty),
            opentelemetry_endpoint: env::var("OPENTELEMETRY_ENDPOINT").ok(),
        }
    }
}

pub struct AppConfig {
    pub port: u16,
    pub telemetry: Telemetry,
}

impl AppConfig {
    pub fn build() -> Self {
        AppConfig {
            port: from_env!("PORT").parse().expect("incorrect PORT"),
            telemetry: Telemetry::build(),
        }
    }
}
