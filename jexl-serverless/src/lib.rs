use std::str::FromStr;

use serde_json::json;
use worker::*;

mod utils;

fn log_request(req: &Request) {
    console_log!(
        "{} - [{}], located at: {:?}, within: {}",
        Date::now().to_string(),
        req.path(),
        req.cf().coordinates().unwrap_or_default(),
        req.cf().region().unwrap_or_else(|| "unknown region".into())
    );
}

#[event(fetch)]
pub async fn main(req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    log_request(&req);

    // Optionally, get more helpful error messages written to the console in the case of a panic.
    utils::set_panic_hook();

    // Optionally, use the Router to handle matching endpoints, use ":name" placeholders, or "*name"
    // catch-alls to match on specific patterns. Alternatively, use `Router::with_data(D)` to
    // provide arbitrary data that will be accessible in each route via the `ctx.data()` method.
    let router = Router::new();

    // Add as many routes as your Worker needs! Each route will get a `Request` for handling HTTP
    // functionality and a `RouteContext` which you can use to  and get route parameters and
    // Environment bindings like KV Stores, Durable Objects, Secrets, and Variables.
    router
        .get("/", |_, _| Response::ok("Hello from Workers!"))
        .post_async("/evaluate", |mut req, _ctx| async move {
            let form = req.form_data().await?;

            match form.get("context") {
                Some(FormEntry::Field(value)) => {
                    let context = serde_json::Value::from_str(&value)
                        .map_err(|e| {
                            console_error!("Invalid context: {}, defaulting to null", e);
                        })
                        .unwrap_or(serde_json::Value::Null);
                    // console_debug!("context: {}", &value);
                    match form.get("expression") {
                        Some(FormEntry::Field(value)) => {
                            let expression = value;
                            let evaluator = jexl_3000::build_evaluator();
                            match evaluator.eval_in_context(&expression, &context) {
                                Ok(res) => Response::from_json(&json!({ "result": &res })),
                                Err(e) => Response::from_json(&json!({
                                    "error": "Evaluation error",
                                    "message": e.to_string()
                                }))
                                .map(|res| res.with_status(422)),
                            }
                        }
                        _ => Response::error("Bad Request", 400),
                    }
                }
                _ => Response::error("Bad Request", 400),
            }
            .map(|res| {
                let cors = Cors::default().with_origins(["*"]);
                res.with_cors(&cors).expect("failed to set cors")
            })
        })
        .get("/worker-version", |_, ctx| {
            let version = ctx.var("WORKERS_RS_VERSION")?.to_string();
            Response::ok(version)
        })
        .run(req, env)
        .await
}
