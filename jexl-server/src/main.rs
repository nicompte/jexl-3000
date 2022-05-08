#![warn(clippy::disallowed_types)]

#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

use anyhow::Result;

mod app_config;
mod error;
mod evaluator;
mod http;
#[cfg(feature = "prom")]
mod metrics;
mod telemetry;

use app_config::AppConfig;

#[tokio::main]
async fn main() -> Result<()> {
    // console_subscriber::init();

    dotenv::dotenv().ok();
    let config = AppConfig::build();

    telemetry::init(&config)?;

    http::serve(config).await?;

    Ok(())
}
