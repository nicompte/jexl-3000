use std::time::Instant;

use axum::{extract::MatchedPath, http::Request, middleware::Next, response::IntoResponse};
use metrics_exporter_prometheus::{Matcher, PrometheusBuilder, PrometheusHandle};

#[cfg(target_os = "linux")]
lazy_static::lazy_static! {
    // getconf CLK_TCK
    static ref CLK_TCK: i64 = {
        unsafe {
            libc::sysconf(libc::_SC_CLK_TCK)
        }
    };

    // getconf PAGESIZE
    static ref PAGESIZE: i64 = {
        unsafe {
            libc::sysconf(libc::_SC_PAGESIZE)
        }
    };

    static ref START_TIME: i64 = {
        if let Ok(boot_time) = procfs::boot_time_secs() {
            if let Ok(p) = procfs::process::Process::myself() {
               p.stat.starttime as i64 / *CLK_TCK + boot_time as i64
            } else {
                0
            }
        } else {
            0
        }
    };
}

pub fn setup_metrics_recorder() -> PrometheusHandle {
    const EXPONENTIAL_SECONDS: &[f64] = &[
        0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0,
    ];

    PrometheusBuilder::new()
        .set_buckets_for_metric(
            Matcher::Full("http_requests_duration_seconds".to_string()),
            EXPONENTIAL_SECONDS,
        )
        .unwrap()
        .install_recorder()
        .expect("failed to install metricss recorder")
}

pub async fn track_metrics<B>(req: Request<B>, next: Next<B>) -> impl IntoResponse {
    let start = Instant::now();
    let path = if let Some(matched_path) = req.extensions().get::<MatchedPath>() {
        matched_path.as_str().to_owned()
    } else {
        req.uri().path().to_owned()
    };
    let method = req.method().clone();

    let response = next.run(req).await;

    let latency = start.elapsed().as_secs_f64();
    let status = response.status().as_u16().to_string();

    let labels = [
        ("method", method.to_string()),
        ("path", path),
        ("status", status),
    ];

    metrics::increment_counter!("http_requests_total", &labels);
    metrics::histogram!("http_requests_duration_seconds", latency, &labels);

    response
}

pub async fn render_metrics(handle: PrometheusHandle) -> impl IntoResponse {
    metrics::gauge!("up", 1.0);
    let _ = get_proc_metrics();

    handle.render()
}

// from https://github.com/tikv/rust-prometheus/blob/master/src/process_collector.rs
#[cfg(target_os = "linux")]
fn get_proc_metrics() -> Result<(), ()> {
    let pid = unsafe { libc::getpid() };
    let p = match procfs::process::Process::new(pid) {
        Ok(p) => p,
        Err(..) => return Ok(()),
    };
    if let Ok(fd_count) = p.fd_count() {
        metrics::gauge!("process_open_fds", fd_count as f64);
    }
    if let Ok(limits) = p.limits() {
        if let procfs::process::LimitValue::Value(max) = limits.max_open_files.soft_limit {
            metrics::gauge!("process_max_fds", max as f64);
        }
    }
    metrics::gauge!("process_virtual_memory_bytes", p.stat.vsize as f64);
    metrics::gauge!(
        "process_resident_memory_bytes",
        (p.stat.rss * *PAGESIZE) as f64
    );
    metrics::gauge!("process_threads", p.stat.num_threads as f64);
    metrics::gauge!("start_time", *START_TIME as f64);

    let total = (p.stat.utime + p.stat.stime) / *CLK_TCK as u64;
    // not really sure about this one...
    metrics::gauge!("process_cpu_seconds_total", total as f64);

    Ok(())
}

#[cfg(not(target_os = "linux"))]
fn get_proc_metrics() -> Result<(), ()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::http::tests::build_app;

    use axum::{body::Body, http::Request};
    use hyper::StatusCode;
    use tower::ServiceExt;

    #[tokio::test]
    async fn e2e_test_basic_metrics() {
        let app = build_app().await;
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/metrics")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let body = String::from_utf8(Vec::from(&body[..])).unwrap();
        assert!(body.contains("up 1"));
        // no route was called so no metrics should be present
        assert!(!body.contains("/config"));
    }

    #[tokio::test]
    async fn e2e_route_metrics() {
        let app = build_app().await;
        // add metrics for the /config route
        let _response = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/evaluate")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/metrics")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let body = String::from_utf8(Vec::from(&body[..])).unwrap();
        assert!(body.contains("up 1"));
        assert!(body.contains("/evaluate"));
        assert!(!body.contains("/metrics"));
    }
}
