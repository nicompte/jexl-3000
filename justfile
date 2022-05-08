alias bi := build-image
alias rc := run-container
alias sc := start-container
alias stc := stop-container
alias rmc := remove-container
alias gen := parser

test:
  cargo nextest run

run:
  cargo run --bin jexl-server

run-release:
  cargo run --release --bin jexl-server

run-release-otel:
  cargo run --release --bin jexl-server --features otel

parser:
  cargo run --bin parser-gen

build-wasm:
  wasm-pack build ./jexl-wasm --target web --release --no-default-features --features wee_alloc,console_error_panic_hook

build-tree-sitter:
  cd tree-sitter-jexl3000 && tree-sitter generate && tree-sitter build-wasm && mv tree-sitter-jexl3000.wasm ../jexl-demo/public/tree-sitter-jexl3000.wasm

build-napi:
  cd jexl-node && RUSTFLAGS="-C target-cpu=native" npm run build

test-wasm:
  cd jexl-wasm && wasm-pack test --firefox

publish:
  cd jexl-serverless && wrangler publish

deploy:
  just build-wasm && cd jexl-demo && npx vite build && netlify deploy --prod

run-otel:
  cargo run --release --bin jexl-server --features otel

build-image:
  docker build . -t jexl-3000-server

run-container:
  docker run --env-file .env.docker --name jexl-3000-server -p 5000:5000 jexl-3000-server

start-container: _start-container _logs

stop-container:
  docker stop jexl-3000-server

remove-container:
  docker container rm jexl-3000-server

_logs:
  docker logs jexl-3000-server -f

_start-container:
  docker start jexl-3000-server