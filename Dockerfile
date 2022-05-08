FROM rust:alpine as builder

RUN apk add --no-cache \
    musl-dev protobuf-dev openssl-dev

WORKDIR /app

# mandatory workspace members
COPY jexl-3000 jexl-3000
COPY jexl-server jexl-server

# other workspace members, can we remove them?
COPY cli cli
COPY jexl-eval jexl-eval
COPY jexl-node jexl-node
COPY jexl-parser jexl-parser
COPY parser-gen parser-gen
COPY jexl-serverless jexl-serverless
COPY jexl-wasm jexl-wasm

# roo Carto.toml & Cargo.lock
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock

RUN cargo build -p jexl-server --release

RUN strip /app/target/release/jexl-server

FROM alpine:latest

COPY --from=builder /app/target/release/jexl-server /

CMD ["./jexl-server"]